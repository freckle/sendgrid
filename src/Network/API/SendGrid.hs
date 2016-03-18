-- | Simple top-level module which re-exports the modules dealing with each endpoint
module Network.API.SendGrid
  ( module Network.API.SendGrid.SendEmail
  , module Network.API.SendGrid.Core
  ) where

import Network.API.SendGrid.SendEmail
import Network.API.SendGrid.Core
