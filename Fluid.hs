module Fluid where

import OpenCL

dotProduct :: InputBuffer CFloat -> InputBuffer CFloat -> OpenCL CFloat
dotProduct vec1 vec2 = 
    multipliedOutMem 

