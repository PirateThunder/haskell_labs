data Vector2D x = Vector2D x x deriving (Show)

scalar (Vector2D x1 y1) (Vector2D x2 y2) = Vector2D (x1*x2) (y1*y2)

add (Vector2D x1 y1) (Vector2D x2 y2) = Vector2D (x1+x2) (y1+y2)

mult (Vector2D x y) num = Vector2D (x*num) (y*num)
