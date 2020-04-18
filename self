thm a->a as self
proof
  inst Ax2 with b=b->a, c=a as E1
  inst Ax1 with b=b->a as E2
  mp E1 and E2 as E3
  mp Ax1 and E3 as goal
end
.
