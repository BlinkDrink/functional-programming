module Lib where

import Data.List (find, group, sort)
import Data.Maybe (isJust)

type Name = String

type Destination = String

type Points = Integer

type Duration = Integer

type Price = Float

data Status = Upcoming | Canceled | InProgress | Finished
  deriving (Eq, Show)

data Activity = MkActivity {activityName :: Name, activityDuration :: Duration}
  deriving (Eq, Show)

data Policy = Flexible | Moderate | Strict
  deriving (Eq, Show)

data Trip = MkTrip
  { destination :: Destination,
    price :: Price,
    status :: Status,
    activities :: [Activity],
    policy :: Policy
  }
  deriving (Eq, Show)

data Customer = MkCustomer {customerName :: Name, customerPoints :: Points, customerTrips :: [Trip]}
  deriving (Eq, Show)

newtype Agency = MkAgency {agencyCustomers :: [Customer]}
  deriving (Eq, Show)

data Thresholds = Thresholds
  { moneyThreshold :: Price,
    durationThreshold :: Duration,
    loyaltyPointsThreshold :: Points
  }

sg :: Ord a => [a] -> [[a]]
sg = group . sort

allUnique :: Ord a => [a] -> Bool
allUnique = all ((==) 1 . length) . sg

freeTripEligibleCustomers :: Agency -> Thresholds -> [Customer]
freeTripEligibleCustomers agency thresholds = filter isCustomerEligible (agencyCustomers agency)
  where
    customerDuration customer = sum $ concat [map activityDuration $ activities x | x <- customerTrips customer]
    customerMoney customer = sum (map price $ customerTrips customer)
    customerCanceled customer = length [x | x <- customerTrips customer, status x == Canceled]
    customerHasVisitedUniqueLocations customer = allUnique [destination x | x <- customerTrips customer]
    isCustomerEligible customer =
      customerDuration customer > durationThreshold thresholds
        && customerMoney customer > moneyThreshold thresholds
        && customerPoints customer > loyaltyPointsThreshold thresholds
        && customerHasVisitedUniqueLocations customer
        && customerCanceled customer == 0

getRefund :: Agency -> Name -> Destination -> Maybe Price
getRefund agency custName dest = case customer of
  Nothing -> Nothing
  Just cust -> case findTripForCustomer cust of
    Just trip -> Just $ refund cust trip
    _ -> Nothing
  where
    customer = case filter (\i -> customerName i == custName) (agencyCustomers agency) of
      [] -> Nothing
      (x : xs) -> Just x

    findTripForCustomer cust = find (\i -> destination i == dest) (customerTrips cust)
    tripPrice cust = case find (\i -> destination i == dest) (customerTrips cust) of -- price
      Just trip -> Just $ price trip
      _ -> Nothing
    customerNonCanceledTrips cust = length [x | x <- customerTrips cust, status x /= Canceled] -- number of all non canceled trips for customer
    customerTotalTrips cust = length $ customerTrips cust -- number of all trips for customer
    k :: Customer -> Float
    k cust = fromIntegral (customerNonCanceledTrips cust) / fromIntegral (customerTotalTrips cust) -- k
    c :: Trip -> Float
    c trip = case policy trip of
      Flexible -> 0.7 :: Float
      Moderate -> 0.4 :: Float
      Strict -> 0.1 :: Float
    totalTrips cust = length $ customerTrips cust

    canceledAvg :: Customer -> Float
    canceledAvg cust = sum (map price $ customerTrips cust) / fromIntegral (totalTrips cust)

    quotient :: Float
    quotient =
      fromIntegral
        ( length
            ( filter
                isJust
                [find (\i -> destination i == dest && status i == Canceled) (customerTrips x) | x <- agencyCustomers agency]
            )
        )
        / fromIntegral (length (filter isJust [find (\i -> destination i == dest) (customerTrips x) | x <- agencyCustomers agency]))

    fee :: Customer -> Float
    fee cust = quotient * canceledAvg cust

    refund :: Customer -> Trip -> Float
    refund cust trip = max (k cust * c trip - fee cust) 0