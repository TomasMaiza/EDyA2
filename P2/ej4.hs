--a) 
--if true then false else true where false = True; true = False
--Esta bien

--b) if if then then else else
-- Esta mal, es un error sintactico. El segundo if debe ir despues del primer else

--c) False ≡ (5 > 4)
--Esta bien, devuelve False

--d) 1 < 2 < 3
--Esta mal, es un error de tipos, al hacer la primer comparacion se obtiene un bool, y luego se compara este bool co un int (esto esta mal)

--e) 1 + if (’a’ < ’z’) then − 1 else 0
--Esta bien, devuelve 0

--f) if fst p then fst p else snd p where p = (True, 2)
--Está mal, es un error de tipo porque then devuelve un Bool y else un Int

--g) if fst p then fst p else snd p where p = (True, False)
--Ta bien (y)