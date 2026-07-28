volatile int in0, in1, in2, in3;
static int compute_body(void)
{
    int a, b, c, d;
    a = in0; b = in1; c = in2; d = in3;

    /* 1 ~ 32 */
    if (a > 0) { a = a + 1; } else { a = a - 1; }
    if (b > 0) { b = b + 2; } else { b = b - 2; }
    if (c > 0) { c = c + 3; } else { c = c - 3; }
    if (d > 0) { d = d + 4; } else { d = d - 4; }

    if (a < 100) { a = a * 2; } else { a = a / 2; }
    if (b < 100) { b = b * 2; } else { b = b / 2; }
    if (c < 100) { c = c * 2; } else { c = c / 2; }
    if (d < 100) { d = d * 2; } else { d = d / 2; }

    if (a >= -50) { a = a + 5; } else { a = a - 5; }
    if (b >= -50) { b = b + 6; } else { b = b - 6; }
    if (c >= -50) { c = c + 7; } else { c = c - 7; }
    if (d >= -50) { d = d + 8; } else { d = d - 8; }

    if (a <= 200) { a = a ^ 1; } else { a = a | 1; }
    if (b <= 200) { b = b ^ 2; } else { b = b | 2; }
    if (c <= 200) { c = c ^ 3; } else { c = c | 3; }
    if (d <= 200) { d = d ^ 4; } else { d = d | 4; }

    if (a == 0) { a = 111; } else { a = a - 10; }
    if (b == 0) { b = 222; } else { b = b - 10; }
    if (c == 0) { c = 333; } else { c = c - 10; }
    if (d == 0) { d = 444; } else { d = d - 10; }

    if (a != 5) { a = a + 9; } else { a = a - 9; }
    if (b != 5) { b = b + 10; } else { b = b - 10; }
    if (c != 5) { c = c + 11; } else { c = c - 11; }
    if (d != 5) { d = d + 12; } else { d = d - 12; }

    if ((a + b) > c) { a = a + 13; } else { a = a - 13; }
    if ((b + c) > d) { b = b + 14; } else { b = b - 14; }
    if ((c + d) > a) { c = c + 15; } else { c = c - 15; }
    if ((d + a) > b) { d = d + 16; } else { d = d - 16; }

    if ((a - b) < d) { a = a + 17; } else { a = a - 17; }
    if ((b - c) < a) { b = b + 18; } else { b = b - 18; }
    if ((c - d) < b) { c = c + 19; } else { c = c - 19; }
    if ((d - a) < c) { d = d + 20; } else { d = d - 20; }

    /* 33 ~ 64 */
    if (a & 1) { a = a + 21; } else { a = a - 21; }
    if (b & 1) { b = b + 22; } else { b = b - 22; }
    if (c & 1) { c = c + 23; } else { c = c - 23; }
    if (d & 1) { d = d + 24; } else { d = d - 24; }

    if ((a | b) > 10) { a = a + 25; } else { a = a - 25; }
    if ((b | c) > 10) { b = b + 26; } else { b = b - 26; }
    if ((c | d) > 10) { c = c + 27; } else { c = c - 27; }
    if ((d | a) > 10) { d = d + 28; } else { d = d - 28; }

    if ((a & b) < 60) { a = a + 29; } else { a = a - 29; }
    if ((b & c) < 60) { b = b + 30; } else { b = b - 30; }
    if ((c & d) < 60) { c = c + 31; } else { c = c - 31; }
    if ((d & a) < 60) { d = d + 32; } else { d = d - 32; }

    if (a > b) { a = a >> 1; } else { a = a << 1; }
    if (b > c) { b = b >> 1; } else { b = b << 1; }
    if (c > d) { c = c >> 1; } else { c = c << 1; }
    if (d > a) { d = d >> 1; } else { d = d << 1; }

    if (a <= b) { a = a + 33; } else { a = a - 33; }
    if (b <= c) { b = b + 34; } else { b = b - 34; }
    if (c <= d) { c = c + 35; } else { c = c - 35; }
    if (d <= a) { d = d + 36; } else { d = d - 36; }

    if (a >= b) { a = a + 37; } else { a = a - 37; }
    if (b >= c) { b = b + 38; } else { b = b - 38; }
    if (c >= d) { c = c + 39; } else { c = c - 39; }
    if (d >= a) { d = d + 40; } else { d = d - 40; }

    if (a != b) { a = a + 41; } else { a = a - 41; }
    if (b != c) { b = b + 42; } else { b = b - 42; }
    if (c != d) { c = c + 43; } else { c = c - 43; }
    if (d != a) { d = d + 44; } else { d = d - 44; }

    if (a == b) { a = a + 45; } else { a = a - 45; }
    if (b == c) { b = b + 46; } else { b = b - 46; }
    if (c == d) { c = c + 47; } else { c = c - 47; }
    if (d == a) { d = d + 48; } else { d = d - 48; }

    /* 65 ~ 96 */
    if ((a + 7) > (b - 2)) { a = a + 49; } else { a = a - 49; }
    if ((b + 7) > (c - 2)) { b = b + 50; } else { b = b - 50; }
    if ((c + 7) > (d - 2)) { c = c + 51; } else { c = c - 51; }
    if ((d + 7) > (a - 2)) { d = d + 52; } else { d = d - 52; }

    if ((a ^ b) > 5) { a = a + 53; } else { a = a - 53; }
    if ((b ^ c) > 5) { b = b + 54; } else { b = b - 54; }
    if ((c ^ d) > 5) { c = c + 55; } else { c = c - 55; }
    if ((d ^ a) > 5) { d = d + 56; } else { d = d - 56; }

    if (a > -100) { a = a + 57; } else { a = a - 57; }
    if (b > -100) { b = b + 58; } else { b = b - 58; }
    if (c > -100) { c = c + 59; } else { c = c - 59; }
    if (d > -100) { d = d + 60; } else { d = d - 60; }

    if (a < 300) { a = a + 61; } else { a = a - 61; }
    if (b < 300) { b = b + 62; } else { b = b - 62; }
    if (c < 300) { c = c + 63; } else { c = c - 63; }
    if (d < 300) { d = d + 64; } else { d = d - 64; }

    if ((a * 1) > (b >> 1)) { a = a + 65; } else { a = a - 65; }
    if ((b * 1) > (c >> 1)) { b = b + 66; } else { b = b - 66; }
    if ((c * 1) > (d >> 1)) { c = c + 67; } else { c = c - 67; }
    if ((d * 1) > (a >> 1)) { d = d + 68; } else { d = d - 68; }

    if ((a << 1) < (b + 20)) { a = a + 69; } else { a = a - 69; }
    if ((b << 1) < (c + 20)) { b = b + 70; } else { b = b - 70; }
    if ((c << 1) < (d + 20)) { c = c + 71; } else { c = c - 71; }
    if ((d << 1) < (a + 20)) { d = d + 72; } else { d = d - 72; }

    if (a + b + c > d) { a = a + 73; } else { a = a - 73; }
    if (b + c + d > a) { b = b + 74; } else { b = b - 74; }
    if (c + d + a > b) { c = c + 75; } else { c = c - 75; }
    if (d + a + b > c) { d = d + 76; } else { d = d - 76; }

    if (a - b - c < d) { a = a + 77; } else { a = a - 77; }
    if (b - c - d < a) { b = b + 78; } else { b = b - 78; }
    if (c - d - a < b) { c = c + 79; } else { c = c - 79; }
    if (d - a - b < c) { d = d + 80; } else { d = d - 80; }

    /* 97 ~ 128 */
    if ((a & 0x0F) > 3) { a = a + 81; } else { a = a - 81; }
    if ((b & 0x0F) > 3) { b = b + 82; } else { b = b - 82; }
    if ((c & 0x0F) > 3) { c = c + 83; } else { c = c - 83; }
    if ((d & 0x0F) > 3) { d = d + 84; } else { d = d - 84; }

    if ((a | 0x10) < 200) { a = a + 85; } else { a = a - 85; }
    if ((b | 0x10) < 200) { b = b + 86; } else { b = b - 86; }
    if ((c | 0x10) < 200) { c = c + 87; } else { c = c - 87; }
    if ((d | 0x10) < 200) { d = d + 88; } else { d = d - 88; }

    if ((a ^ 0x07) > 10) { a = a + 89; } else { a = a - 89; }
    if ((b ^ 0x07) > 10) { b = b + 90; } else { b = b - 90; }
    if ((c ^ 0x07) > 10) { c = c + 91; } else { c = c - 91; }
    if ((d ^ 0x07) > 10) { d = d + 92; } else { d = d - 92; }

    if (a / 2 > b / 3) { a = a + 93; } else { a = a - 93; }
    if (b / 2 > c / 3) { b = b + 94; } else { b = b - 94; }
    if (c / 2 > d / 3) { c = c + 95; } else { c = c - 95; }
    if (d / 2 > a / 3) { d = d + 96; } else { d = d - 96; }

    if (a % 4 != 0) { a = a + 97; } else { a = a - 97; }
    if (b % 4 != 0) { b = b + 98; } else { b = b - 98; }
    if (c % 4 != 0) { c = c + 99; } else { c = c - 99; }
    if (d % 4 != 0) { d = d + 100; } else { d = d - 100; }

    if ((a + 11) >= (b + 2)) { a = a + 101; } else { a = a - 101; }
    if ((b + 11) >= (c + 2)) { b = b + 102; } else { b = b - 102; }
    if ((c + 11) >= (d + 2)) { c = c + 103; } else { c = c - 103; }
    if ((d + 11) >= (a + 2)) { d = d + 104; } else { d = d - 104; }

    if ((a - 5) <= (b + 9)) { a = a + 105; } else { a = a - 105; }
    if ((b - 5) <= (c + 9)) { b = b + 106; } else { b = b - 106; }
    if ((c - 5) <= (d + 9)) { c = c + 107; } else { c = c - 107; }
    if ((d - 5) <= (a + 9)) { d = d + 108; } else { d = d - 108; }

    if ((a + b) != (c - d)) { a = a + 109; } else { a = a - 109; }
    if ((b + c) != (d - a)) { b = b + 110; } else { b = b - 110; }
    if ((c + d) != (a - b)) { c = c + 111; } else { c = c - 111; }
    if ((d + a) != (b - c)) { d = d + 112; } else { d = d - 112; }

    return a ^ b ^ c ^ d;
}

int main(void)
{
    /* Test vector set, cover positive, negative and zero */
    int test_inputs[4][4] = {
        {10, -20, 35, -7},
        {0, 0, 0, 0},
        {123, -45, 67, -89},
        {-50, 99, -1, 42}
    };
    int i;
    int pass = 1;

    for (i = 0; i < 4; i++)
    {
        /* Set volatile input */
        in0 = test_inputs[i][0];
        in1 = test_inputs[i][1];
        in2 = test_inputs[i][2];
        in3 = test_inputs[i][3];

        /* Run twice, if-conversion must preserve semantics */
        int r1 = compute_body();
        in0 = test_inputs[i][0];
        in1 = test_inputs[i][1];
        in2 = test_inputs[i][2];
        in3 = test_inputs[i][3];
        int r2 = compute_body();

        if (r1 != r2)
        {
            return -1;
        }
    }
    return 0;
}

