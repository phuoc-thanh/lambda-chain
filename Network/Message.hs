{-# LANGUAGE OverloadedStrings #-}

module Network.Message where

import Network.Connection    

-- | Raw, Request , Response
data Msg = Raw | Req | Res

data Req = Block | Chain | Transaction
