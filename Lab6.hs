module Lab6 where 
 data RegularPoly = RegularPoly Int Float
 
 polyArea (RegularPoly nSides length) = ((length**2)*(fromIntegral nSides))/(4*(tan(pi/(fromIntegral nSides))))
 
 instance Eq RegularPoly where
  (RegularPoly nSides1 length1) == (RegularPoly nSides2 length2) = abs(polyArea(RegularPoly nSides1 length1) - polyArea(RegularPoly nSides2 length2)) < 0.001

 instance Show RegularPoly where
  show(RegularPoly nSides length) = do
   let shapes = ["","","","Equilateral Triangle","Square","Regular Pentagon", "Regular Hexagon", "Regular Heptagon", "Regular Octagon", "Regular Nonagon", "Regular Decagon", "Regular Undecagon", "Regular Dodecagon"]
   if nSides < 13 then "< " ++ shapes!!nSides ++" > < Number of sides: " ++ show nSides ++" > < Length of side: "++ show length++ " >"
   else "< Regular Polygon > < Number of sides: " ++ show nSides ++" > < Length of side: "++ show length++ " >"
 