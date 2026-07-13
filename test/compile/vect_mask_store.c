char output[1024];
char data[1024];
int scale;
int threshold;
void main()
{
    //Only update the data if condition is true.
    for (int i=0; i<1024; i++) {
      if (data[i] > threshold) {//sparse condition.
        output[i] = data[i] * scale;
      }
    }
}
