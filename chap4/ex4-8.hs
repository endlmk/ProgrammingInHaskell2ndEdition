luhnDouble n = if d > 9 then d - 9 else d
                where d = n * 2

luhn x y z w = mod s 10 == 0
               where s = luhnDouble x + y + luhnDouble z + w