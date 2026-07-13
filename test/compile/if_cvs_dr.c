int fnear(float x, float y)
{
  float t = x - y;
  return t == 0 || x / t > 1000000.0;
}
