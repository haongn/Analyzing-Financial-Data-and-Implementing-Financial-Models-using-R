ZCB = function(r, T, t, m, FV)
{
  # This function calculates current value Z(t, T) of a zero-coupon bond with known FV.
  # Note: t and T here is in terms of years
  if (m == "inf")
  {
    Z = FV / exp(r * (T - t))
  }
  else 
  {
    Z = FV / (1 + r/m)**(m*(T - t))
  }
}