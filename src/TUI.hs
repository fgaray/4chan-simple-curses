{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
module TUI where


import Chan


import Prelude
import qualified Prelude as P
import Brick
import qualified Brick.Types as T
import qualified Brick.Main as M
import qualified Brick.Util as U
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import Brick.Widgets.Core
import qualified Brick.AttrMap as A
import qualified Brick.Widgets.Border as B
import qualified Brick.Focus as F
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Dialog as D
import qualified Graphics.Vty as V
import qualified Data.Vector as Vec
import Lens.Micro
import Data.Maybe
import Control.Monad.IO.Class
import Data.List
import Text.HTML.TagSoup
import Data.String.Utils as H


data BoardName =
      BoardG
    | BoardPol
    | BoardFit
    | BoardBiz
    deriving (Enum, Bounded)

instance Show BoardName where
    show BoardG   = "g"
    show BoardPol = "pol"
    show BoardFit = "fit"
    show BoardBiz = "biz"


data CurrentState =
      CurrentInit
    | CurrentCatalog BoardName [ThreadCatalog]
    | CurrentThread BoardName ThreadNumber Thread [ThreadJSON] [ThreadJSON] [ThreadJSON]
    | CurrentImage String CurrentState
    deriving Show
    

data Name = 
      Command
    | ListAttrs
    | Post Int
  deriving (Ord, Show, Eq)


data State = State
    { current       :: CurrentState
    , listBoards    :: L.List Name BoardName
    , listCatalog   :: L.List Name ThreadCatalog
    }


initialState :: State
initialState = State
    CurrentInit
    (L.list ListAttrs (Vec.fromList [minBound..maxBound]) 1)
    (L.list ListAttrs Vec.empty 1)

drawUI :: State -> [T.Widget Name]
drawUI State{..} =
    case current of
        CurrentInit    -> [boards]
        CurrentCatalog board _ -> [catalog board]
        CurrentThread board number thread focussed _ replies ->
            if null replies then [th board number thread focussed]
                else [B.borderWithLabel (str "Replies") $ th board number thread replies]
        CurrentImage img _ -> [image img]
    where
        boards = B.border . C.center $ L.renderList listDrawElement True listBoards

        catalog board = B.borderWithLabel (str $ show board) $ L.renderList listDrawElement True listCatalog

        th board number (Thread posts) focussed = vBox $ map (showPost board focussed) focussed
    
        listDrawElement :: Show a => Bool -> a -> T.Widget Name
        listDrawElement True x  = str $ ">>" ++ show x
        listDrawElement False x = str $ show x

        showPost :: BoardName -> [ThreadJSON] -> ThreadJSON -> T.Widget Name
        showPost board ts t@ThreadJSON{..} =
            let rsp = findReplies' (show no) ts
            in
            B.borderWithLabel (str $ genName t ++ ", rsp: " ++ show (length rsp)) $
            strWrap $ (fromMaybe "[EMPTY]" $ fmap stripTags com)
                ++ "\n" ++ createUrl board t


        genName ThreadJSON{..} =
            "[" ++ show no ++ "] "
            ++ name
            ++ (fromMaybe "" . fmap (\s -> "(" ++ s ++ ")") $ country_name)

        image = B.border . str
        

createUrl board ThreadJSON{..} =
    case tim of
        Nothing -> ""
        Just t -> "http://i.4cdn.org/" ++ show board ++ "/" ++ show t ++ fromJust ext



stripTags :: String -> String
stripTags = foldl' (\acc x -> acc ++ renderTagsOptions renderOptions{optEscape = P.id} [x] ++ " ") ""
    . filter fn
    . parseTags
    . H.replace "<br>" "\n"
    where
        fn :: Tag String -> Bool
        fn (TagText _) = True
        fn _ = False


appEvent :: State -> T.BrickEvent Name e -> T.EventM Name (T.Next State)
appEvent state@State{..} (T.VtyEvent ev) =
    case (current, ev) of
        (CurrentInit, V.EvKey V.KEsc []) -> M.halt state
        (CurrentCatalog _ _, V.EvKey V.KEsc []) -> M.continue $ state { current = CurrentInit}
        (CurrentThread _ _ _ _ _ _, V.EvKey V.KEnter []) -> goToCatalog
        (CurrentThread a b c d e xs, V.EvKey V.KEsc []) -> if null xs then goToCatalog else M.continue $ state { current = CurrentThread a b c d e [] }
        (CurrentImage _ cur, V.EvKey V.KEsc []) -> M.continue $ state { current = cur }
        (CurrentInit, V.EvKey V.KEnter []) -> goToCatalog
        (CurrentInit, _) ->
            L.handleListEventVi L.handleListEvent ev listBoards
            >>= M.continue . \l -> state { listBoards = l}
        (CurrentCatalog _ _, V.EvKey (V.KChar 'r') []) -> goToCatalog
        (CurrentCatalog board cat, V.EvKey V.KEnter []) -> goToThread board cat
        (CurrentCatalog _ _, _) ->
            L.handleListEventVi L.handleListEvent ev listCatalog
            >>= M.continue . \l -> state { listCatalog = l}
        (CurrentThread board no thread (x:xs) ys [], V.EvKey (V.KChar 'j') []) ->
            M.continue $ state { current = CurrentThread board no thread xs (x:ys) []}
        (CurrentThread board no thread xs (y:ys) [], V.EvKey (V.KChar 'k') []) ->
            M.continue $ state { current = CurrentThread board no thread (y:xs) ys []}
        (CurrentThread board no thread (x:xs) ys [], V.EvKey (V.KChar 'o') []) -> do
            img <- liftIO $ downloadImage (createUrl board x) (fromJust $ ext x)
            M.continue $ state { current = CurrentImage img (CurrentThread board no thread (x:xs) ys [])}
        (CurrentThread board no' thread (x@ThreadJSON{..}:xs) ys [], V.EvKey (V.KChar 'q') []) -> do
            let rsp = findReplies' (show no) xs
            M.continue $ state { current = CurrentThread board no' thread (x:xs) ys rsp}
        (CurrentThread _ _ _ _ _ _, _) -> M.continue state
        (CurrentImage _ cur, _) -> M.continue $ state { current = cur }

    where
        boardSelected :: BoardName
        boardSelected = [minBound..maxBound] !! (fromJust $ listBoards ^. L.listSelectedL)

        threadSelected :: [ThreadCatalog] -> ThreadCatalog
        threadSelected cat = cat !! (fromJust $ listCatalog ^. L.listSelectedL)
        
        errorToString :: Show a => Either a b -> Either String b
        errorToString (Left x) = Left $ show x
        errorToString (Right x) = Right x

        goToCatalog = do
            catalog <- fmap errorToString . liftIO $ run (getCatalog $ show boardSelected)
            case catalog of
                Left err -> M.continue $ state { current = CurrentCatalog boardSelected [] }
                Right cat -> do
                    let ts   = sortBy (\t1 t2 -> compare (last_modified t2) (last_modified t1)) . concat . map threads $ cat
                        list = L.listReplace (Vec.fromList ts) (Just 0) listCatalog
                    M.continue $ state { current = CurrentCatalog boardSelected ts, listCatalog = list }
            
        goToThread board cat = do
            let t = threadSelected cat
            (no, thread) <-
                case t of
                    ThreadCatalog _ _ _ _ _ no _ ->
                        fmap (\x -> (ThreadNumber no, errorToString x))
                        . liftIO $ run (getThread (show board) (ThreadNumber no))
            case thread of
                Left err -> M.halt state
                Right t -> do
                    M.continue $ state { current = CurrentThread board no t (posts t) [] []}

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr []


app :: M.App State e Name
app = M.App
    { M.appDraw         = drawUI
    , M.appChooseCursor = \_ _ -> Nothing
    , M.appHandleEvent  = appEvent
    , M.appStartEvent   = return
    , M.appAttrMap      = const theMap
    }



tuiMain :: IO ()
tuiMain = do
    state <- M.defaultMain app initialState
    return ()

ui :: Widget ()
ui = str "Hola mundo"

