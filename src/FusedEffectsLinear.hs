{- |
Copyright: (c) 2021 Patrick Thomson
SPDX-License-Identifier: BSD-3-Clause
Maintainer: Patrick Thomson <patrickt@github.com>

Fused effects with linearity.
-}

module FusedEffectsLinear
       ( someFunc
       ) where


someFunc :: IO ()
someFunc = putStrLn ("someFunc" :: String)
