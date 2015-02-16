# TODO: do this in Haskell instead
winboth = 0
split = 0
loseboth = 0
for a1 in range(1,7):
  for a2 in range(1,7):
    for a3 in range(1,7):
      for d1 in range(1,7):
        for d2 in range(1,7):
          amin, amax = sorted([a1,a2,a3])[1:3]
          dmax = max(d1,d2)
          dmin = min(d1,d2)
          if amax > dmax and amin > dmin:
            winboth += 1
          elif amin <= dmin and amax <= dmax:
            loseboth += 1
          else:
            split +=1

print(winboth)
print(split)
print(loseboth)