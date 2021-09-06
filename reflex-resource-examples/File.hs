module File (
    IO.IOMode(..),
    IO.FilePath,
    openFile,
    hGetLine
) where

import Control.Monad.IO.Class
import Reflex.Resource
import Reflex.Resource.Allocate
import Reflex.Resource.Unsafe

import qualified Data.Text as T
import qualified Data.Text.IO as IOT
import qualified System.IO as IO
import System.IO.Error

import RIO

openFile :: (ResourceContext rp r, MonadIO m) => IO.FilePath -> IO.IOMode -> RIOT r t m (Res r (Maybe IO.Handle))
openFile path mode = allocate $ newResource (liftIO $ catchIOError (Just <$> IO.openFile path mode)
                                                                   (const $ return Nothing))
                                            (\handle -> ( return ()
                                                        , case handle of
                                                               Just h -> liftIO $ IO.hClose h
                                                               Nothing -> return ()
                                                        ))

hGetLine :: (MonadIO m, MonadUseResource r r' m) => Res r IO.Handle -> m T.Text
hGetLine h = liftIO $ catchIOError (IOT.hGetLine (unRes h)) (const $ return T.empty)
