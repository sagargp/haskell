-- module Units where 

data MetricUnit   = Meter | Liter  | Kilogram deriving (Show, Eq)
data ImperialUnit = Yard  | Gallon | Pound deriving (Show, Eq)
data Measurement  = MetricMeasurement Double MetricUnit
                  | ImperialMeasurement Double ImperialUnit
                  deriving Show

symbol :: MetricUnit -> String
symbol k | k == Meter    = "m"
         | k == Liter    = "L"
         | k == Kilogram = "kg"

convert :: Measurement -> Measurement
convert (MetricMeasurement quantity unit)
        | unit == Meter    = ImperialMeasurement (1.09361 * quantity) Yard
        | unit == Liter    = ImperialMeasurement (0.26417 * quantity) Gallon
        | unit == Kilogram = ImperialMeasurement (2.20462 * quantity) Pound
convert (ImperialMeasurement quantity unit)
        | unit == Yard   = MetricMeasurement (quantity / 1.09361) Meter
        | unit == Gallon = MetricMeasurement (quantity / 0.26417) Liter
        | unit == Pound  = MetricMeasurement (quantity / 2.20462) Kilogram

main = print (convert (convert (MetricMeasurement 3 Meter)))
