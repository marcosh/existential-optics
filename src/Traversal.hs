module Traversal where

import Optics
import PowerSeries

-- DATA TYPE

type Traversal = Optic PowerSeries
