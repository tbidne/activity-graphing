module App where

import Control.Monad.Trans.Writer.Lazy

newtype App = App {
  runApp :: WriterT String IO ()
}