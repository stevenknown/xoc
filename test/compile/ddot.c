double ddot(int n, double dx[], int dx_off, int incx,
            double dy[],
            int dy_off, int incy)
{
    double dtemp;
    int i,ix,iy;
    dtemp = 0;
    if (n > 0) {
      if (incx != 1 || incy != 1) {
    // code for unequal increments or equal increments not equal to 1
    ix = 0;
    iy = 0;
    if (incx < 0) ix = (-n+1)*incx;
    if (incy < 0) iy = (-n+1)*incy;
    for (i = 0;i < n; i++) {
      dtemp += dx[ix +dx_off]*dy[iy +dy_off];
      ix += incx;
      iy += incy;
    }
      } else {
    // code for both increments equal to 1
    for (i=0;i < n; i++)
      dtemp += dx[i +dx_off]*dy[i +dy_off];
      }
    }
    return(dtemp);
}


void f(char p[], char q[])
{
    p[1]=q[1]; //p, q may alias.
    p[1]=0;
    q[1]=0; //
}
