{-# OPTIONS_GHC -Wall #-}

module Main where

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk
import Graphics.Rendering.Chart.Backend.Cairo

import Data.Colour
import Data.Colour.Names
import Control.Lens
import Data.Default.Class
import System.Environment(getArgs)

chart = toRenderable layout
  where
    circle = [ (r a * sin (a*dr),r a * cos (a*dr)) | a <- [0,0.5..360::Double] ]
      where
        dr = 2 * pi / 360
        r a = 0.8 * cos (a * 20 * pi /360)

    circleP = plot_lines_values .~ [circle]
            $ plot_lines_style .~ solidLine 1.0 (opaque blue)
            $ def

    layout = layout_title .~ "Parametric Plot"
           $ layout_plots .~ [toPlot circleP]
           $ def

main = renderableToWindow chart 200 200
