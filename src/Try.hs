{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Try () where

import Control.Lens hiding (element)
import Control.Monad.State.Strict
import Data.List ((!?))

----------------------------------------------------------
-- 1. Lenses Primer
--
-- Lenses are about **focusing on a piece of a structure**.
--
-- Plain field accessors (_location, _x, etc.) already behave
-- like “getter lenses”: they are just functions, and functions
-- **compose with (.)**. So getter composition works out of the box.
--
-- But setters do **not** compose — once you update a field, you
-- must manually rebuild all the surrounding structure.
--
-- Lenses fix this by giving us composable getters *and*
-- composable setters/modifiers, using the same Haskell (.) operator.
----------------------------------------------------------

data Location = Location
  { _x :: Double      -- Use underscores because we will
  , _y :: Double }    -- generate `x` and `y` later.
  deriving (Show, Eq)

data Atom = Atom
  { _element  :: String
  , _location :: Location }
  deriving (Show, Eq)


----------------------------------------------------------
-- Manual get/set/modify
----------------------------------------------------------

-- --- GET (manual) ---
-- Getter composition works because field accessors are just functions.
-- This is **ordinary Haskell function composition**.
getX_non :: Atom -> Double
getX_non = _x . _location   -- :: Atom -> Double

-- --- SET (manual) ---
-- But setter composition does NOT work — we must reconstruct by hand.
setX_non :: Double -> Atom -> Atom
setX_non new (Atom e (Location _ y')) =
  Atom e (Location new y')

-- --- MODIFY (manual) ---
shiftAtomX_non :: Atom -> Atom
shiftAtomX_non (Atom e (Location x' y')) =
  Atom e (Location (x' + 1) y')


----------------------------------------------------------
-- Now let TH create proper lenses for each field.
----------------------------------------------------------

makeLenses ''Location
-- Generates:
--     x :: Lens' Location Double
--     y :: Lens' Location Double

makeLenses ''Atom
-- Generates:
--     element  :: Lens' Atom String
--     location :: Lens' Atom Location


----------------------------------------------------------
-- With lenses:
--   ✓ composable getters
--   ✓ composable setters
--   ✓ composable modifiers
-- All using the SAME (.) operator you already know.
----------------------------------------------------------

-- --- GET with lenses ---
getX :: Atom -> Double
getX = view (location . x)    -- composed lens: still (.)

getX_fancy :: Atom -> Double
getX_fancy a = a ^. (location . x)


-- --- SET with lenses ---
setX :: Double -> Atom -> Atom
setX = set (location . x)     -- setter now composes cleanly

setX_fancy :: Double -> Atom -> Atom
setX_fancy newX a = (location . x) .~ newX $ a


-- --- MODIFY with lenses ---
shiftAtomX :: Atom -> Atom
shiftAtomX = over (location . x) (+1)

shiftAtomX_fancy :: Atom -> Atom
shiftAtomX_fancy = (location . x) %~ (+1)
----------------------------------------------------------
-- 2. Inside a State
--
-- Same idea as before, but now lenses let us focus INTO a
-- large state record. Instead of unpack/repack in `modify`,
-- we use lens operators (.=, %=, +=).
--
-- Two extra optics appear here:
--   - `ix i`       : focus on the i-th element of a list (Maybe)
--   - `traversed`  : focus on each element inside a Traversable
--
-- They aren't the focus of the tutorial, but they're useful
-- "index" and "loop" optics that compose just like lenses.
----------------------------------------------------------

data Protein = Protein
  { _pname :: String
  , _atoms :: [Atom]
  } deriving (Show, Eq)

data ST = ST
  { _protein :: Protein
  , _other   :: ()
  } deriving (Show, Eq)

-- TH generates lenses for fields
makeLenses ''Protein
makeLenses ''ST

type M = State ST


----------------------------------------------------------
-- Simple example: renaming the protein
----------------------------------------------------------

renameProtein_non :: String -> M ()
renameProtein_non s = do
  (ST (Protein _ as) x) <- get
  put $ ST (Protein s as) x

renameProtein :: String -> M ()
renameProtein s =
  protein . pname .= s


----------------------------------------------------------
-- Reading the x-coordinate of atom i
----------------------------------------------------------

-- Manual version: unpack state, index the list, dig fields.
readAtomX_non :: Int -> M (Maybe Double)
readAtomX_non i = do
  st <- get
  let ST (Protein _ as) _ = st
  let a = as !? i
  return ((_x . _location) <$> a)

-- Lens version:
--   protein        focus on the Protein field
--   atoms          focus on the list inside it
--   ix i           focus on the i-th element (returns a Maybe focus)
--   location . x   drill further
readAtomX :: Int -> M (Maybe Double)
readAtomX i =
  preuse (protein . atoms . ix i . location . x)


----------------------------------------------------------
-- Setting the x-coordinate of atom i
----------------------------------------------------------

setAtomX_non :: Int -> Double -> M ()
setAtomX_non i v = modify $ \(ST (Protein nm as) x) ->
  let bump (Atom e (Location _ y')) = Atom e (Location v y')
  in ST (Protein nm (updateAt i bump as)) x
  where
    updateAt i f = go i
      where
        go _ []     = []
        go 0 (y:ys) = f y : ys
        go n (y:ys) = y : go (n-1) ys

-- Lens version: directly set the nested Double
setAtomX :: Int -> Double -> M ()
setAtomX i v =
  protein . atoms . ix i . location . x .= v


----------------------------------------------------------
-- Moving a single atom by (dx, dy)
----------------------------------------------------------

moveAtom_non :: Int -> Double -> Double -> M ()
moveAtom_non i dx dy = modify $ \(ST (Protein nm as) x) ->
  let bump (Atom e (Location x' y')) =
        Atom e (Location (x'+dx) (y'+dy))
  in ST (Protein nm (updateAt i bump as)) x
  where
    updateAt i f = go i
      where
        go _ []     = []
        go 0 (y:ys) = f y : ys
        go n (y:ys) = y : go (n-1) ys

-- Lens version: update nested fields directly
moveAtom :: Int -> Double -> Double -> M ()
moveAtom i dx dy = do
  protein . atoms . ix i . location . x %= (+ dx)
  protein . atoms . ix i . location . y += dy


----------------------------------------------------------
-- Moving ALL atoms by (dx, dy)
----------------------------------------------------------

moveAll_non :: Double -> Double -> M ()
moveAll_non dx dy = modify $ \(ST (Protein nm as) x) ->
  let bump (Atom e (Location x' y')) =
        Atom e (Location (x'+dx) (y'+dy))
  in ST (Protein nm (map bump as)) x

-- Here `traversed` walks over each element in the list.
moveAllOrigin :: M ()
moveAllOrigin =
  protein . atoms . traversed . location .= Location 0 0

-- Use traversed to apply a function to each Location
moveAll :: Double -> Double -> M ()
moveAll dx dy =
  protein . atoms . traversed . location %=
    \(Location x' y') -> Location (x'+dx) (y'+dy)


----------------------------------------------------------
-- Replace all Carbon atoms ("C") with Helium
----------------------------------------------------------

-- Manual version: map, test element, replace
heliumifyCarbons_non :: M ()
heliumifyCarbons_non =
  modify $ \(ST (Protein nm as) x) ->
    let step (Atom e loc) =
          Atom (if e == "C" then "He" else e) loc
    in ST (Protein nm (map step as)) x

-- Lens version:
--   traversed      visit every atom
--   filtered       keep only Carbon atoms (Lens for Maybe-like foci)
--   element .=     set element string for the focused ones
heliumifyCarbons :: M ()
heliumifyCarbons =
  protein . atoms . traversed
    . filtered (\a -> a ^. element == "C")
    . element .= "He"


----------------------------------------------------------
-- Demo scripts
----------------------------------------------------------

script_non :: M (Maybe Double)
script_non = do
  x0 <- readAtomX_non 1
  setAtomX_non 0 42
  moveAtom_non 1 2 3
  moveAll_non 1 0
  heliumifyCarbons_non
  renameProtein_non "Demo"
  pure x0

script :: M (Maybe Double)
script = do
  x0 <- readAtomX 1
  setAtomX 0 42
  moveAtom 1 2 3
  moveAll 1 0
  heliumifyCarbons
  renameProtein "Demo"
  pure x0

----------------------------------------------------------
-- 3. Object Oriented
--
-- This section shows how lenses can mimic *OO-style
-- inheritance* by composing records and using “classy lenses”.
--
-- Classy lenses generate:
--   - a typeclass (e.g. HasVehicle)
--   - a top-level lens (vehicle :: Lens' s Vehicle)
--   - lenses for each field (positionV, speed, brand, …)
--
-- Any type that “has a Vehicle” can implement HasVehicle,
-- giving us polymorphic lens-based operations similar to
-- OO methods that work on subclasses.
----------------------------------------------------------

-- Base "class"
data Vehicle = Vehicle
  { _positionV :: Location   -- lenses: positionV, speed, brand
  , _speed     :: Double
  , _brand     :: String
  } deriving (Show, Eq)

-- makeClassy creates:
--   class HasVehicle s where
--     vehicle  :: Lens' s Vehicle   -- focus on whole Vehicle
--
--   positionV :: HasVehicle s => Lens' s Location
--   speed      :: HasVehicle s => Lens' s Double
--   brand      :: HasVehicle s => Lens' s String
makeClassy ''Vehicle


----------------------------------------------------------
-- Car “extends” Vehicle via a has-a relationship.
-- (Car contains a Vehicle field, so we can expose it.)
----------------------------------------------------------

data Car = Car
  { _vehicleC :: Vehicle     -- lens: vehicleC
  , _airLevel :: Double      -- lens: airLevel
  } deriving (Show, Eq)

makeClassy ''Car
-- Generates HasCar, car, airLevel, and so on.

-- Wire Car into HasVehicle:
-- Saying “a Car has a Vehicle” by pointing vehicle at vehicleC.
instance HasVehicle Car where
  vehicle = vehicleC


----------------------------------------------------------
-- ElectricCar extends Car, which extends Vehicle.
-- We expose full inheritance by composing the lenses.
----------------------------------------------------------

data ElectricCar = ElectricCar
  { _carE          :: Car
  , _batteryCharge :: Double
  } deriving (Show, Eq)

makeClassy ''ElectricCar

-- ElectricCar “has a Car”
instance HasCar ElectricCar where
  car = carE

-- ElectricCar also “has a Vehicle”, via Car’s Vehicle.
instance HasVehicle ElectricCar where
  vehicle = car . vehicle     -- ordinary (.) composition again


----------------------------------------------------------
-- Lens-based "methods"
-- These work on ANY type that implements the right Has* class.
----------------------------------------------------------

-- Accelerate by dv
accelerate_non :: Double -> Vehicle -> Vehicle
accelerate_non dv (Vehicle p v b) =
  Vehicle p (v + dv) b

-- Lens version: works for Vehicle, Car, ElectricCar, …
accelerate :: HasVehicle s => Double -> s -> s
accelerate dv =
  speed %~ (+ dv)


-- Pump air into a Car (0–100 clamp)
pumpAir_non :: Double -> Car -> Car
pumpAir_non df (Car veh f) =
  Car veh (max 0 (min 100 (f + df)))

pumpAir :: HasCar s => Double -> s -> s
pumpAir df =
  airLevel %~ (\f -> max 0 (min 100 (f + df)))


-- Move a vehicle by (dx, dy)
move_non :: Double -> Double -> Vehicle -> Vehicle
move_non dx dy (Vehicle (Location x0 y0) v b) =
  Vehicle (Location (x0 + dx) (y0 + dy)) v b

-- Lens version: works for anything with a Vehicle
move :: HasVehicle s => Double -> Double -> s -> s
move dx dy =
    (positionV . x %~ (+ dx))
  . (positionV . y %~ (+ dy))


-- Rename vehicle brand
renameBrand_non :: String -> Vehicle -> Vehicle
renameBrand_non nm (Vehicle p v _) =
  Vehicle p v nm

renameBrand :: HasVehicle s => String -> s -> s
renameBrand nm =
  brand .~ nm


----------------------------------------------------------
-- Demo
-- Note polymorphism: 'move' and 'accelerate' work for Car
-- and ElectricCar automatically via the HasVehicle instances.
----------------------------------------------------------

demoVehicles :: IO ()
demoVehicles = do
  let v  = Vehicle (Location 0 0) 10 "Zoomster"
      c  = Car v 50
      ec = ElectricCar c 30

  -- move / accelerate
  print (move_non 3 4 v)
  print (move 3 4 c)    -- uses Car -> Vehicle lens
  print (accelerate_non 5 v)
  print (accelerate 5 ec)

  -- fuel / charge
  print (pumpAir_non 15 c)
  print (pumpAir 15 ec) -- ElectricCar "is a" Car via HasCar

  -- brand get/set
  print (renameBrand_non "VoltX" v)
  print (renameBrand "VoltX" c)



----------------------------------------------------------
-- 4. Under the hood
--
-- This section peeks at how a lens *really works*.
-- No category theory needed — just the types.
--
-- Recall the actual lens type:
--
--   type Lens s t a b =
--     forall f. Functor f => (a -> f b) -> s -> f t
--
-- A lens takes:
--   - a small “focus function”      (a -> f b)
--   - a larger structure             s
--   and uses the Functor machinery to rebuild a new structure  f t.
--
-- Informally:
--   “If you tell me how to operate on the focused part (a),
--    I’ll lift that into an operation on the whole structure (s).”
--
-- In the general form (s,t,a,b), a lens can change types,
-- but in this tutorial we only use the simpler isomorphic case
--   Lens' s a  = Lens s s a a.
--
-- Below we build tiny lens-like versions to get a feel for the idea.

----------------------------------------------------------


----------------------------------------------------------
-- A baby lens type: only supports modifying (no functor)
----------------------------------------------------------

-- A very simplified “lens”:
--    (b -> b) -> a -> a
-- This is enough to show the basic idea of running a function
-- on the focused field and rebuilding the whole value.
type Lens'' a b = (b -> b) -> a -> a

over'' :: Lens'' a b -> (b -> b) -> a -> a
over'' ln f = ln f

set'' :: Lens'' a b -> b -> a -> a
set'' ln b =
  ln (const b)


----------------------------------------------------------
-- Using Identity to rephrase over
--
-- The real lens type uses a Functor f, so if we plug in
-- Identity as our f, we get modification.
----------------------------------------------------------

over' :: Lens' a b -> (b -> b) -> a -> a
over' ln f a =
  runIdentity (ln (\b -> Identity (f b)) a)


----------------------------------------------------------
-- Using Const to implement view
--
-- View can't be defined in the baby version of our lenses. Do you see
-- why?
--
-- If we choose Const as the Functor, the structure rebuild
-- is ignored, and we only extract the focused value.
----------------------------------------------------------

view'_impossible :: Lens'' a b -> a -> b
view'_impossible ln a = undefined -- Can't be done.

-- Const r a: a functor that ignores the 'a' and keeps 'r'.
-- So feeding Const to a lens extracts the a-part.
view' :: Lens' a b -> a -> b
view' ln a =
  getConst (ln Const a)
