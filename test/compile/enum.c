enum aes_const {
    Nrow = 4, /* the number of rows in the cipher state       */
    Mcol = 8, /* maximum number of columns in the state       */
    Ncol = 4,
    Shr0 = 0, /* the cyclic shift values for rows 0, 1, 2 & 3 */
    Shr1 = 1,
    Shr2 = 2, //Shr2 = 16 == 32 ? 3 : 2,
    Shr3 = 3, //Shr3 = 16 == 32 ? 4 : 3
};

enum aes_const gen;
void f() {
    enum ET a = 1;
    int b = a;
    int cx;
    (cx = Ncol, a++);
}
