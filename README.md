dates
-----

When benchmarking an app, I noticed that 40% of the time and 35% of the memory
expenditure was due to date-munging using the conventional 
Data.Time.Calendar.Day type.  That's just a newtype that wraps Integer.

Since I probably won't have to worry about overflow until around

    ModifiedJulianDay (fromIntegral (maxBound :: Int) :: Integer)
    -- 25252734927768413-06-12

I figured it might be a little more efficient to use a newtype that wraps Int.

