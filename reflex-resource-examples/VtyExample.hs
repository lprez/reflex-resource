{-# LANGUAGE FlexibleContexts, TypeFamilies, RecursiveDo #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.IO.Class
import Data.Maybe
import qualified Data.IntMap as I
import qualified Data.IntMap.Merge.Lazy as I
import qualified Data.Patch.IntMap as I
import qualified Data.Text as T
import Data.Patch (Patch(..))
import Reflex.Resource
import Reflex.Vty

import RIO
import File

instance MonadIO m => MonadIO (Layout t m) where
    liftIO = lift . liftIO

-- The textbox and button to input a file path. Returns an event that fires when
-- the button is clicked and contains the path.
openFileGui :: (MonadVtyApp t m, MonadNodeId m) => Layout t m (Event t T.Text)
openFileGui = fixed (pure 3) . row $
    do text <- fmap _textInput_value (stretch . boxStatic def $ textInput def)
       ev <- fixed (pure 6) (textButtonStatic def $ T.pack "Open")
       return $ tag (current text) ev

-- The buttons to open a file, read from them and close the application.
buttonBarGui :: (MonadVtyApp t m, MonadNodeId m)
             => Dynamic t Bool
             -> Layout t m (Event t T.Text, Event t (), Event t ())
buttonBarGui anySelected =
    do -- Text of the read more button.
       let readMoreText = fmap (\selected -> if selected
                                             then T.pack "Read More"
                                             else T.pack "No file selected") anySelected
       -- The events for the buttons.
       (openEv, readMoreEv, shutdownEv) <-
         fixed (pure 3) . row $ (,,) <$> stretch (textButtonStatic def (T.pack "Open File"))
                                     <*> stretch (textButton def (current readMoreText))
                                     <*> stretch (textButtonStatic def (T.pack "Exit"))
       -- The area displayed when the "Open File" button is clicked.
       rec -- The area is initially hidden, and displayed again depending on the
           -- openCloseGuiEv event.
           (ev0, ev') <- runWithReplace (return never) openCloseGuiEv
           -- Display the area when the button is clicked, don't display anything
           -- when the path has been submitted.
           let openCloseGuiEv = mergeWith const [ fmapCheap (const $ openFileGui) openEv
                                                , fmapCheap (const $ return never) openPathEv ]
           -- Event that contains the path of the file to open.
           openPathEv <- switchDyn <$> holdDyn ev0 ev'
       return (openPathEv, readMoreEv, shutdownEv)

-- A file tab that can be selected, with a button to close it.
fileTab :: (MonadVtyApp t m, MonadNodeId m, ResourceContext rp r)
        => Dynamic t Int
        -> Int
        -> FilePath
        -> Event t Int
        -> RIOT r t (Layout t m) (Event t (), Event t (Dynamic t T.Text))
fileTab selectedIndex idx path readMoreEv =
    do mHandle <- openFile path ReadMode
       withMaybeRes mHandle
                    (\handle ->
                       do -- Read some lines when readMoreEv fires.
                          textUpdateEv <- performEvent $ fforCheap readMoreEv $
                                   \i -> liftIO . fmap T.unlines $ mapM (\_ -> hGetLine handle) [1 .. i]
                          -- Change the tab title when the tab is selected.
                          let tabTitle = fmap (\selected -> if idx == selected
                                                            then T.pack ('*' : path)
                                                            else T.pack path)
                                              selectedIndex
                          -- Close and select button.
                          (closeEv, selectEv) <- liftRIO . stretch . boxStatic roundedBoxStyle . row $
                                               (,) <$> fixed (pure 3)
                                                             (textButtonStatic def (T.pack "X"))
                                                   <*> stretch (textButton def (current tabTitle))
                          textDyn <- holdDyn T.empty textUpdateEv
                          return (closeEv, fmapCheap (const textDyn) selectEv))
                    (return (never, never))

main :: IO ()
main = mainWidget $
    do initEv <- getPostBuild
       -- Run the RIO monad. The underlying monad can either be VtyWidget or Layout, and hoistRIO is used
       -- to switch between the two.
       (shutdownEv, initialization, finalization) <- runRIO $ hoistRIO col $
              do rec -- Event that fires when file is selected and contains the index and the text of the file.
                     let selectTextEv = switchDyn $       fmapMaybeCheap (listToMaybe . I.toAscList)
                                                        . mergeInt
                                                        . fmap (\(_, select) -> select)
                                                    <$> tabEvents
                     selectedText <- holdDyn (-1, pure T.empty) selectTextEv
                     let -- Content of the file that is currently selected.
                         textContent = join $ snd <$> selectedText
                         -- Index of the selected file.
                         selectedIndex = fst <$> selectedText
                         -- Selector for the readMore event of each file.
                         readEvSelector = fanInt $ pushAlways (\_ -> liftA2 I.singleton (sample (current selectedIndex))
                                                                                        (sample (current textHeight)))
                                                              readMoreEv
                         -- An unused IntMap index.
                         nextFileIdx = fmap (maybe 0 ((+1) . fst) . I.lookupMax) tabEvents
                         -- Event that fires when a file is opened and contains the path and the associated
                         -- readMore event in the form of a PatchIntMap addition.
                         openEv = attachWith (\k p -> PatchIntMap $
                                                        I.singleton k (Just ( T.unpack p
                                                                            , selectInt readEvSelector k)))
                                             (current nextFileIdx)
                                             openFileEv
                         -- Event that fires when a file is closed and contains a patch that removes the
                         -- entry from the IntMap.
                         closeEv = fmap PatchIntMap . switchDyn $
                                        mergeInt . fmap (\(close, _) -> fmapCheap (const Nothing) close)
                                    <$> tabEvents
                     -- Dynamic IntMap containings the events for each tab (close and select).
                     tabEvents <- foldDynMaybe apply tabEvents0 tabEvents'
                     -- The file tab bar.
                     (tabEvents0, tabEvents') <- hoistRIO (fixed (pure 5) . row) $
                       traverseIntMapWithKeyWithAdjustContext (\k (path, readEv) ->
                                                                    fileTab selectedIndex k path readEv)
                                                              I.empty
                                                              (openEv <> closeEv)

                     -- Whether any file is selected.
                     let anySelected = I.member <$> selectedIndex <*> tabEvents
                     -- The area in which the text is displayed and the button bar.
                     (textHeight, (openFileEv, readMoreEv, shutdownEv)) <- liftRIO $
                            (,) <$> (stretch $ displayHeight <* text (current textContent))
                                <*> buttonBarGui anySelected

                 return $ pure shutdownEv
       -- Initialization (note that, even though openFile doesn't use it,
       -- the library might require it for other purposes, like deallocating
       -- resources from withTmpContext).
       performEvent_ $ fmapCheap (const $ liftIO initialization) initEv
       -- Finalization (closes any file that was left open).
       performEvent_ $ pushAlwaysCheap (const $ liftIO <$> finalization) shutdownEv
       return shutdownEv
