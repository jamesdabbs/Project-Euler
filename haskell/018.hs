-- Finds the largest sum along routes to the bottom of the triangle
-- solution = 1074
triangle = map words [
    "75",
    "95 64",
    "17 47 82",
    "18 35 87 10",
    "20 04 82 47 65",
    "19 01 23 75 03 34",
    "88 02 77 73 07 63 67",
    "99 65 04 28 06 16 70 92",
    "41 41 26 56 83 40 80 70 33",
    "41 48 72 33 47 32 37 16 94 29",
    "53 71 44 65 25 43 91 52 97 51 14",
    "70 11 33 28 77 73 17 78 39 68 17 57",
    "91 71 52 38 17 14 91 43 58 50 27 29 48",
    "63 66 04 68 89 53 67 30 73 16 69 87 40 31",
    "04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"]
    
depth = (length triangle) - 1

-- A 'route' will be a choice of 0 or 1 at each of the 14 steps down the triangle
lExp list 1 = list
lExp list n = [a ++ b | a<-list, b<-lExp list (n-1)]

routes = lExp ["0", "1"] depth

routeSum :: Int -> Int -> Int

routeSum row column = if row==depth
    then read (triangle!!row!!column)
    else read (triangle!!row!!column) +
        max (routeSum (row+1) column) (routeSum (row+1) (column+1))
