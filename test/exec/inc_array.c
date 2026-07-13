int printf(char const*,...);
int main()
{
   int i;
   int count[10];
   int count2[10];

   count2[0]=11;
   count2[1]=12;
   count2[2]=13;

   count[0]=-2;
   count[1]=1;
   count[2]=2;
   count[3]=3;
   count[4]=4;
   count[5]=5;
   count[6]=6;
   count[7]=8;
   count[8]=9;
   count[9]=10;
   printf("\nbefore");
   for(i=0; i<10;i++){
     printf("\ncount[%d]=%d", i, count[i]);
   }
   for(i=1; ++count[i]>i&&i<10;){
      printf("\n...");
      count[i]=0;
      count2[i++]=0;
   }

   printf("\n\nafter");
   for(i=0; i<10;i++){
     printf("\ncount[%d]=%d", i, count[i]);
   }
   printf("\n");
   return 0;
}
