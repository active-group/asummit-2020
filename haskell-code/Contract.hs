module Contract where

type Amount = Int 
data Currency = GBP | EUR | USD
data Date = Date String
  deriving (Eq, Ord)

-- Zero-Coupon Bond
-- Receive 100€ on 21 Jan 2021
data Contract =
      Zero
    | One Currency
--    | Multiple Amount Currency
    | Multiple Amount Contract -- Kombinator!
    | Later Date Contract -- den Vertrag später abschließen
    | Invert Contract
    | Both Contract Contract
 --   | ZCB Amount Currency Date

--- Kombinator

-- Foo -> ... -> Foo

-- zcb1'' = ZCB 100 EUR (Date "2021-01-21")
zcb1' = Later (Date "2021-01-21") (Multiple 100 (One EUR))

zcb amount currency date =
    Later date (Multiple amount (One currency))

zcb1 = zcb 100 EUR (Date "2021-01-21")
zcb2 = zcb 100 GBP (Date "2022-01-21")

cc = Both zcb1 (Invert zcb2)

c1 = One EUR -- Ich bekomme 1€ jetzt
c2 = Multiple 100 (One EUR)
c3 = Multiple 20 c2

c4 = Invert zcb2 -- "Pay 100GBP on 2022-01-21"

c5 = Both c1 c4
c6 = Both c5 c3

data Payment = Payment Amount Currency

scale :: Amount -> (Payment -> Payment)
scale amount1 (Payment amount2 currency) =
    Payment (amount1 * amount2) currency

listMap :: (a -> b) -> [a] -> [b]
listMap f [] = []
listMap f (first:rest) = 
    (f first) : listMap f rest

-- Was ist mit dem Vertrag bis jetzt passiert?
evolve :: Date -> Contract -> (Contract, [Payment])
evolve date Zero = (Zero, [])
evolve date (One currency) = (Zero, [Payment 1 currency])
evolve date (Multiple factor contract) = 
    let (contractAfter, payments) = evolve date contract
    in (Multiple factor contractAfter,
        listMap (scale factor) payments)
evolve date (Later date' contract) = 
    if date >= date'
    then evolve date contract
    else (Later date' contract, [])
evolve date (Both contract1 contract2) = 
    let (contract1', payments1) = evolve date contract1
        (contract2', payments2) = evolve date contract2
    in (Both contract1' contract2',
        payments1 ++ payments2)