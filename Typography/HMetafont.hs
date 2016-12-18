{-# LANGUAGE DeriveGeneric, DeriveTraversable, DeriveDataTypeable,
    GeneralizedNewtypeDeriving, ImplicitParams, TemplateHaskell,
    LambdaCase, TupleSections, MultiParamTypeClasses #-}
module Typography.HMetafont
  ( 
    -- * expressions
    Expr, MFVar, MFConst, var, constant, xpart, ypart,
    left, right, up, down, cosd, sind, angle, dir,
    between, betweenPt, topPt, botPt, lftPt, rtPt, top,
    bot, lft, rt, 
    -- * transformations
    scaled, xscaled, yscaled, shifted, zscaled, rotated,
    rotateAround, rotateAbout, slanted, identity, transformed,
    xxpart, xypart, txpart, yxpart, yypart, typart,
    
    -- * actions:
    MFAction, mfError, mfWarning, getParam, evalStruct, evalExpr, whatever,
    -- ** drawing on the canvas
    Pen, evalPen, fill, draw, fillDraw, eraseFill, eraseDraw, eraseFillDraw,
    -- * equations
    known, (===), (=&=), unifyAll,
    -- * paths
    Tension(..), (.--.), (-.), (.-), tension,
    tensionAtLeast, tensions, leaving, arriving, startCurl, endCurl,
    controls, cyclePath, endpt,
    
    -- * pens
    
    -- * Geom2D (imported)
    module Geom2D, 
    -- * predefined variables: 
    x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11,
    x12, x13, x14, x15, x16, x17, x18, x19,
    x1l, x2l, x3l, x4l, x5l, x6l, x7l, x8l, x9l, x10l, x11l,
    x12l, x13l, x14l, x15l, x16l, x17l, x18l, x19l,
    x1r, x2r, x3r, x4r, x5r, x6r, x7r, x8r, x9r, x10r, x11r,
    x12r, x13r, x14r, x15r, x16r, x17r, x18r, x19r,
    y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11,
    y12, y13, y14, y15, y16, y17, y18, y19,
    y1l, y2l, y3l, y4l, y5l, y6l, y7l, y8l, y9l, y10l, y11l,
    y12l, y13l, y14l, y15l, y16l, y17l, y18l, y19l,
    y1r, y2r, y3r, y4r, y5r, y6r, y7r, y8r, y9r, y10r, y11r,
    y12r, y13r, y14r, y15r, y16r, y17r, y18r, y19r,
    z1, z2, z3, z4, z5, z6, z7, z8, z9, z10, z11,
    z12, z13, z14, z15, z16, z17, z18, z19,
    z1l, z2l, z3l, z4l, z5l, z6l, z7l, z8l, z9l, z10l, z11l,
    z12l, z13l, z14l, z15l, z16l, z17l, z18l, z19l,
    z1r, z2r, z3r, z4r, z5r, z6r, z7r, z8r, z9r, z10r, z11r,
    z12r, z13r, z14r, z15r, z16r, z17r, z18r, z19r

   ) where
import Geom2D.CubicBezier 
import Geom2D
import Text.Printf
import Control.Lens
import Data.Hashable
import Control.Applicative
import Data.Functor
import Data.Foldable
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Except
import System.Random
import qualified Math.MFSolve as S
import GHC.Generics
import GHC.Stack

-- | An internal type for representing variables in expressions.
data MFVar = StringVar String
           | UniqueVar Integer
           deriving (Ord, Eq, Generic)

instance Show MFVar where
  show (StringVar s) = s
  show (UniqueVar i) = printf "unique<%d>" i

-- | @MFAction num params retval@: An action type that handles solving
-- equations and constraints, drawing or erasing on the current
-- canvas, reading extra parameters, retrieving information about
-- metafont variables, etc...
--
-- @num@ is the type of numbers, `Double` for normal font instances,
-- and an (unimplemented) symbolic type for font variations.  If you
-- want to support both, make sure you use a typevariable here.
-- However Double offers more flexibility, so use that if you don't
-- mind losing font variation support.
--
-- @params@ is the type of extra parameters passed to the action.
-- This is useful for parameters which aren't expressions (for example
-- Boolean arguments).
--
-- @retval@ is the type of the return value of the action.
newtype MFAction e r a = MFAction (ReaderT r (StateT (MFState e) (Either String)) a)
  deriving (Functor, Applicative, Monad, MonadState (MFState e),
            MonadReader r, MonadError String)

data Pen a = Pen a
  deriving (Functor, Foldable, Traversable, Ord, Eq)

data PathOp e = FillPath (MFPath e)
              | DrawPath (MFPath e) (Pen e)
              | ErasePath (MFPath e)
              | EraseDrawPath (MFPath e) (Pen e)

data MFPath e =
  OpenMFPath (OpenMetaPath e) |
  ClosedMFPath (ClosedMetaPath e)

-- | an algebraic expression in the metafont sense, containing
-- constants, variables, and operations on them.  Since they are an
-- instance of the standard numeric typeclasses, standard numeric
-- functions work on them.
newtype Expr e = Expr (S.Expr MFVar e)
  deriving (Floating, Fractional, Num, Generic)

instance (Num e, Ord e, Show e) => Show (Expr e)
  where show (Expr e) = show e

instance Hashable MFVar
instance Hashable e => Hashable (Expr e)

-- | A typeclass for representing constants.  The main use for this
-- typeclass is to support both font variations and regular instances.
class (Hashable a, RealFrac a, Ord a, Show a, Floating a) => MFConst a where
  setMax :: Expr a -> Expr a -> Expr a -> MFAction e r ()
  setMin :: Expr a -> Expr a -> Expr a -> MFAction e r ()
  penDir :: Pen a -> Point a -> Point a

instance MFConst Double where
  penDir = undefined
  setMax = undefined
  setMin = undefined

data PartialPath e = PartialOpen (MetaJoin e) (OpenMetaPath e) | 
                     PartialClosed (MetaJoin e) (ClosedMetaPath e)

data MFState e = MFState {
  _mfDeps :: S.Dependencies MFVar e,
  _mfPaths :: [PathOp e],
  _mfWarnings :: [String],
  _lastUnique :: Integer
  }

makeLenses ''MFState

cyclePath :: MFPath a
cyclePath = ClosedMFPath (ClosedMetaPath [])

endpt :: Point a -> MFPath a
endpt p = OpenMFPath (OpenMetaPath [] p)

infixr 7 -.
infixr 7 .-
infixr 7 .--.

type JoinModifier e = MetaJoin e -> MetaJoin e  
  
defaultJoin :: Num e => MetaJoin e  
defaultJoin = MetaJoin Open (Tension 1) (Tension 1) Open
  
(-.) :: Num e => JoinModifier e -> MFPath e -> PartialPath e
f -. OpenMFPath o =
  PartialOpen (f defaultJoin) o

f -. ClosedMFPath c =
  PartialClosed (f defaultJoin) c

(.-) :: Point a -> PartialPath a -> MFPath a
p .- (PartialClosed j (ClosedMetaPath joins)) =
  ClosedMFPath (ClosedMetaPath ((p,j):joins))

p .- (PartialOpen j (OpenMetaPath joins l)) =
  OpenMFPath (OpenMetaPath ((p,j):joins) l)

(.--.) :: (Num a) => Point a -> MFPath a -> MFPath a
p .--. OpenMFPath (OpenMetaPath joins lst) =
  OpenMFPath (OpenMetaPath ((p, defaultJoin):joins) lst)

p .--. ClosedMFPath (ClosedMetaPath joins) =
  ClosedMFPath (ClosedMetaPath ((p, defaultJoin):joins))

tension :: a -> JoinModifier a
tension t mj = mj {tensionL = Tension t, tensionR = Tension t}

tensionAtLeast :: a -> JoinModifier a
tensionAtLeast t mj = mj {tensionL = TensionAtLeast t, tensionR = TensionAtLeast t}

tensions :: Tension a -> Tension a -> JoinModifier a
tensions t u mj = mj {tensionL = t, tensionR = u}

leaving, arriving :: Point a -> JoinModifier a
leaving l mj = mj {metaTypeL = Direction l}
arriving r mj = mj {metaTypeR = Direction r}

startCurl, endCurl :: a -> JoinModifier a
startCurl r mj = mj {metaTypeL = Curl r}
endCurl r mj = mj {metaTypeR = Curl r}

controls :: Point a -> Point a -> JoinModifier a
controls p q _ = Controls p q

lastCallStack :: CallStack -> CallStack
lastCallStack s = case getCallStack s of
  [] -> emptyCallStack
  l -> fromCallSiteList [last l]

-- | Generate an error with a calltrace
mfError :: HasCallStack => String -> MFAction e r a
mfError s = throwError $
            prettyCallStack (lastCallStack callStack) ++ " " ++ s

-- | Generate a warning with a calltrace
mfWarning :: HasCallStack => String -> MFAction e r ()
mfWarning s = modifying mfWarnings
              ((prettyCallStack (lastCallStack callStack) ++ " " ++ s) :)

-- lift a MFSolve compatible monad into the MFAction monad.  Return Either a S.DepError or the value.
liftSolver :: StateT (S.Dependencies MFVar e) (Either (S.DepError MFVar e)) a
           -> MFAction e r (Either (S.DepError MFVar e) a)
liftSolver m = do
  deps <- use mfDeps
  case runStateT m deps of
    Left err -> return $ Left err
    Right (a,s) -> do
      mfDeps .= s
      return $ Right a

-- | A variable expression with label.  Depending on the current
-- system of equations, the variable can have on of three states:
--
--   * /Known/: the variable has a numeric value
--   * /Dependend/: the variable has a linear dependency on other (unknown) variables
--   * /Unknown/: the variable isn't /Known/ or /dependend/.  Unlike in 
--      metafont, this library allows variables to have a nonlinear
--      dependency on other variables.  The equation is simply suspended
--      until it becomes a linear equation, after which it's handles like other equations.
var :: (Num e) => String -> Expr e
var = Expr . S.makeVariable . StringVar

-- | Create an expression from a constant
constant :: e -> Expr e
constant = Expr . S.makeConstant

x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11,
  x12, x13, x14, x15, x16, x17, x18, x19,
  x1l, x2l, x3l, x4l, x5l, x6l, x7l, x8l, x9l, x10l, x11l,
  x12l, x13l, x14l, x15l, x16l, x17l, x18l, x19l,
  x1r, x2r, x3r, x4r, x5r, x6r, x7r, x8r, x9r, x10r, x11r,
  x12r, x13r, x14r, x15r, x16r, x17r, x18r, x19r :: Num e => Expr e
x1 = var "x1"; x1l = var "x1l"; x1r = var "x1r"
x2 = var "x2"; x2l = var "x2l"; x2r = var "x2r"
x3 = var "x3"; x3l = var "x3l"; x3r = var "x3r"
x4 = var "x4"; x4l = var "x4l"; x4r = var "x4r"
x5 = var "x5"; x5l = var "x5l"; x5r = var "x5r"
x6 = var "x6"; x6l = var "x6l"; x6r = var "x6r"
x7 = var "x7"; x7l = var "x7l"; x7r = var "x7r"
x8 = var "x8"; x8l = var "x8l"; x8r = var "x8r"
x9 = var "x9"; x9l = var "x9l"; x9r = var "x9r"
x10 = var "x10"; x10l = var "x10l"; x10r = var "x10r"
x11 = var "x11"; x11l = var "x11l"; x11r = var "x11r"
x12 = var "x12"; x12l = var "x12l"; x12r = var "x12r"
x13 = var "x13"; x13l = var "x13l"; x13r = var "x13r"
x14 = var "x14"; x14l = var "x14l"; x14r = var "x14r"
x15 = var "x15"; x15l = var "x15l"; x15r = var "x15r"
x16 = var "x16"; x16l = var "x16l"; x16r = var "x16r"
x17 = var "x17"; x17l = var "x17l"; x17r = var "x17r"
x18 = var "x18"; x18l = var "x18l"; x18r = var "x18r"
x19 = var "x19"; x19l = var "x19l"; x19r = var "x19r"

y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11,
  y12, y13, y14, y15, y16, y17, y18, y19,
  y1l, y2l, y3l, y4l, y5l, y6l, y7l, y8l, y9l, y10l, y11l,
  y12l, y13l, y14l, y15l, y16l, y17l, y18l, y19l,
  y1r, y2r, y3r, y4r, y5r, y6r, y7r, y8r, y9r, y10r, y11r,
  y12r, y13r, y14r, y15r, y16r, y17r, y18r, y19r :: Num e => Expr e
y1 = var "y1"; y1l = var "y1l"; y1r = var "y1r"
y2 = var "y2"; y2l = var "y2l"; y2r = var "y2r"
y3 = var "y3"; y3l = var "y3l"; y3r = var "y3r"
y4 = var "y4"; y4l = var "y4l"; y4r = var "y4r"
y5 = var "y5"; y5l = var "y5l"; y5r = var "y5r"
y6 = var "y6"; y6l = var "y6l"; y6r = var "y6r"
y7 = var "y7"; y7l = var "y7l"; y7r = var "y7r"
y8 = var "y8"; y8l = var "y8l"; y8r = var "y8r"
y9 = var "y9"; y9l = var "y9l"; y9r = var "y9r"
y10 = var "y10"; y10l = var "y10l"; y10r = var "y10r"
y11 = var "y11"; y11l = var "y11l"; y11r = var "y11r"
y12 = var "y12"; y12l = var "y12l"; y12r = var "y12r"
y13 = var "y13"; y13l = var "y13l"; y13r = var "y13r"
y14 = var "y14"; y14l = var "y14l"; y14r = var "y14r"
y15 = var "y15"; y15l = var "y15l"; y15r = var "y15r"
y16 = var "y16"; y16l = var "y16l"; y16r = var "y16r"
y17 = var "y17"; y17l = var "y17l"; y17r = var "y17r"
y18 = var "y18"; y18l = var "y18l"; y18r = var "y18r"
y19 = var "y19"; y19l = var "y19l"; y19r = var "y19r"

z1, z2, z3, z4, z5, z6, z7, z8, z9, z10, z11,
  z12, z13, z14, z15, z16, z17, z18, z19,
  z1l, z2l, z3l, z4l, z5l, z6l, z7l, z8l, z9l, z10l, z11l,
  z12l, z13l, z14l, z15l, z16l, z17l, z18l, z19l,
  z1r, z2r, z3r, z4r, z5r, z6r, z7r, z8r, z9r, z10r, z11r,
  z12r, z13r, z14r, z15r, z16r, z17r, z18r, z19r :: Num e => Point (Expr e)

z1 = Point x1 y1; z1l = Point x1l y1l; z1r = Point x1r y1r
z2 = Point x2 y2; z2l = Point x2l y2l; z2r = Point x2r y2r
z3 = Point x3 y3; z3l = Point x3l y3l; z3r = Point x3r y3r
z4 = Point x4 y4; z4l = Point x4l y4l; z4r = Point x4r y4r
z5 = Point x5 y5; z5l = Point x5l y5l; z5r = Point x5r y5r
z6 = Point x6 y6; z6l = Point x6l y6l; z6r = Point x6r y6r
z7 = Point x7 y7; z7l = Point x7l y7l; z7r = Point x7r y7r
z8 = Point x8 y8; z8l = Point x8l y8l; z8r = Point x8r y8r
z9 = Point x9 y9; z9l = Point x9l y9l; z9r = Point x9r y9r
z10 = Point x10 y10; z10l = Point x10l y10l; z10r = Point x10r y10r
z11 = Point x11 y11; z11l = Point x11l y11l; z11r = Point x11r y11r
z12 = Point x12 y12; z12l = Point x12l y12l; z12r = Point x12r y12r
z13 = Point x13 y13; z13l = Point x13l y13l; z13r = Point x13r y13r
z14 = Point x14 y14; z14l = Point x14l y14l; z14r = Point x14r y14r
z15 = Point x15 y15; z15l = Point x15l y15l; z15r = Point x15r y15r
z16 = Point x16 y16; z16l = Point x16l y16l; z16r = Point x16r y16r
z17 = Point x17 y17; z17l = Point x17l y17l; z17r = Point x17r y17r
z18 = Point x18 y18; z18l = Point x18l y18l; z18r = Point x18r y18r
z19 = Point x19 y19; z19l = Point x19l y19l; z19r = Point x19r y19r

-- | return a unique variable
whatever :: (Num e) => MFAction e r (Expr e)
whatever = do
  u <- use lastUnique
  modifying lastUnique (+1)
  return $ Expr $ S.makeVariable $ UniqueVar u

-- | get the extra parameters.
getParam :: MFAction e r r
getParam = ask

-- | replace all expressions in the structure with its numeric value
-- (if possible).  This function cannot be used for variable fonts.
evalStruct :: (HasCallStack, Traversable t) => t (Expr Double) -> MFAction Double r (t Double)
evalStruct = evalStruct'

evalStruct' :: (HasCallStack, MFConst e, Traversable t) => t (Expr e) -> MFAction e r (t e)
evalStruct' = traverse evalExpr'

-- | return the numeric value of the expression.  This function cannot
-- be used for variable fonts.
evalExpr :: HasCallStack => Expr Double -> MFAction Double r Double
evalExpr = evalExpr'

-- | evaluate the pen, since pen drawing operations expect numeric pens.
evalPen :: (HasCallStack, MFConst e) => Pen (Expr e) -> MFAction e r (Pen e)
evalPen = evalStruct'

evalExpr' :: (MFConst e, HasCallStack) => Expr e -> MFAction e r e
evalExpr' (Expr e) =
  liftSolver (S.getExpr e) >>= \case
    Left _ -> mfError $ " Couldn't reduce expression " ++ show e ++ " to number"
    Right v -> return v

-- | return if the expression evaluates to a numeric value
known :: (MFConst a) => Expr a -> MFAction a r Bool
known e =
  (evalExpr' e >> return True)
  `catchError` const (return False)

-- | Make the two expressions equal to each other.
(===) :: (MFConst e, HasCallStack)
      => Expr e -> Expr e -> MFAction e r ()
(Expr a) === (Expr b) =
  liftSolver (a S.=== b) >>= \case
    Left (S.RedundantEq _) ->
        mfWarning $ "Redundant equation: " ++
        show a ++ " === " ++ show b
    Left (S.InconsistentEq _ _) ->
        mfError $ "Inconsistent equation: " ++ show a ++ " === " ++ show b
    Left _ -> mfError "Unexpected error."
    Right _ -> return ()
  
-- | Make the two points equal to each other.
(=&=) :: (MFConst e, HasCallStack)
      => Point (Expr e) -> Point (Expr e) -> MFAction e r ()
pa@(Point (Expr ax) (Expr ay)) =&= pb@(Point (Expr bx) (Expr by)) =
  liftSolver ((ax, ay) S.=&= (bx, by)) >>= \case
    Left (S.RedundantEq _) ->
      mfWarning $ "Redundant equation: " ++
      show pa ++ " =&= " ++ show pb
    Left (S.InconsistentEq _ _) ->
           mfError $ "Inconsistent equation: " ++ show pa ++ " =&= " ++ show pb
    Left _ -> mfError "Unexpected error."
    Right _ -> return ()
  
-- | Unify all expressions in the list
unifyAll :: (MFConst e, HasCallStack) => [Expr e] -> MFAction e r ()
unifyAll [] = mfWarning "unifying empty list"
unifyAll [_] = mfWarning "unifying singleton list"
unifyAll (e:l) = traverse_ (e ===) l

cosd, sind :: Floating a => a -> a
-- | cosine in degrees
cosd t = cos(t*pi/180)
-- | sine in degrees
sind t = sin(t*pi/180)

-- | direction of angle (in degrees)
dir :: Floating a => a -> Point a
dir t = Point (cosd t) (sind t)

-- | angle of vector (in degrees)
angle :: RealFloat a => Point a -> a
angle (Point x y) = 180 * atan2 y x / pi

up, down, left, right :: Num a => Point a
-- | unit vector pointing up
up = Point 0 1

-- | unit vector pointing down
down = Point 0 (-1)

-- | unit vector pointing left
left = Point (-1) 0

-- | unit vector pointing right
right = Point 1 0

xpart, ypart :: Point a -> a
-- | return X-coordinate of point
xpart = pointX
-- | return Y-coordinate of point
ypart = pointY

scaled, xscaled, yscaled, slanted :: (AffineTransform a b, Num b) => a -> b -> a
scaled x s = Transform s 0 0 0 s 0 $* x
xscaled x s = Transform s 0 0 0 1 0 $* x
yscaled x s = Transform 1 0 0 0 s 0 $* x

shifted :: (AffineTransform a b, Num b) => Point b -> a -> a
shifted x p = translate x $* p

zscaled :: (AffineTransform a b, Num b) => a -> Point b -> a
zscaled x (Point u v) = Transform u (-v) 0 v u 0 $* x

slanted x s = Transform 1 s 0 0 1 0 $* x

rotated :: (AffineTransform t e, Floating e) => t -> e -> t
rotated x t = Transform (cosd t) (negate $ sind t)
              0 (sind t) (cosd t) 0 $* x
rotateAround, rotateAbout :: Floating b => Point b -> Point b -> b -> Point b
rotateAround x p t = x `shifted` (p ^* (-1)) `rotated` t `shifted` p
rotateAbout = rotateAround

identity :: Num a => Transform a
identity = idTrans

transformed :: AffineTransform t e => t -> Transform e -> t
transformed = flip ($*)

xxpart, xypart, txpart, yxpart, yypart, typart :: Transform a -> a
xxpart = xformA
xypart = xformB
txpart = xformC
yxpart = xformD
yypart = xformE
typart = xformF

-- | interpolate between numbers
between :: Num a => a -> a -> a -> a
between e1 e2 e3 = e2 * (1-e1) + (e3 * e1)

-- | interpolate between points
betweenPt :: Num a => a -> Point a -> Point a -> Point a
betweenPt s p q = (1-s) *^ p ^+^ s *^ q

fill :: MFPath e -> MFAction e r ()
fill p =
  modifying mfPaths (FillPath p:)

draw :: Pen e -> MFPath e -> MFAction e r ()
draw pen path =
  modifying mfPaths (DrawPath path pen:)

fillDraw :: Pen e -> MFPath e -> MFAction e r ()
fillDraw pen path =
  modifying mfPaths $ \ops ->
  FillPath path: DrawPath path pen:ops

eraseFill :: MFPath e -> MFAction e r ()
eraseFill p =
  modifying mfPaths (ErasePath p:)

eraseDraw :: Pen e -> MFPath e -> MFAction e r ()
eraseDraw pen path =
  modifying mfPaths (EraseDrawPath path pen:)

eraseFillDraw :: Pen e -> MFPath e -> MFAction e r ()
eraseFillDraw pen path =
  modifying mfPaths $ \ops ->
  ErasePath path: EraseDrawPath path pen:ops

topPt, botPt, lftPt, rtPt :: (MFConst e) => Pen e -> Point (Expr e) -> Point (Expr e)

-- | return topmost point on a pen at the given point
topPt pen pt = pt ^+^ fmap constant (penDir pen (Point 0 1))

-- | return bottom most point on a pen at the given point
botPt pen pt = pt ^+^ fmap constant (penDir pen (Point 0 (-1)))

-- | return left most point on a pen at the given point
lftPt pen pt = pt ^+^ fmap constant (penDir pen (Point (-1) 0))

-- | return right most point on a pen at the given point
rtPt  pen pt = pt ^+^ fmap constant (penDir pen (Point 1 0))

top, bot, lft, rt :: (MFConst e) => Pen e -> Expr e -> Expr e

-- | return topmost coordinate on a pen at the given point
top pen y = y + constant (pointY $ penDir pen $ Point 0 1)

-- | return bottom most coordinate on a pen at the given point
bot pen y = y + constant (pointY $ penDir pen $ Point 0 (-1))

-- | return left most coordinate on a pen at the given point
lft pen x = x + constant (pointX $ penDir pen $ Point (-1) 0)

-- | return right most coordinate on a pen at the given point
rt  pen x = x + constant (pointX $ penDir pen $ Point 1 0)

ifLt a b c d = undefined

ifGt a b c d = undefined
