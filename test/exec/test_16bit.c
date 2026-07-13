extern void exit(int);
extern void abort();

//********** 16bit shift testing **********/
int test_16bit_lsr ()
{
    int errors = 0;
    unsigned int results[128];
    unsigned int x[] = {-1, 0x7fff, 0, 1, 0x8000};
    x[0]=-1;
    x[1]=0x7fff;
    x[2]=0;
    x[3]=1;
    x[4]=0x8000;
    unsigned int i, j;

    for (i = 0; i < sizeof(x)/sizeof(x[0]); i++) {
        results[0] = (x[i] >> (0));
        results[1] = (x[i] >> (1));
        results[2] = (x[i] >> (2));
        results[3] = (x[i] >> (3));
        results[4] = (x[i] >> (4));
        results[5] = (x[i] >> (5));
        results[6] = (x[i] >> (6));
        results[7] = (x[i] >> (7));
        results[8] = (x[i] >> (8));
        results[9] = (x[i] >> (9));
        results[10] = (x[i] >> (10));
        results[11] = (x[i] >> (11));
        results[12] = (x[i] >> (12));
        results[13] = (x[i] >> (13));
        results[14] = (x[i] >> (14));
        results[15] = (x[i] >> (15));
        results[16] = (x[i] >> (16));
        results[17] = (x[i] >> (17));
        results[18] = (x[i] >> (18));
        results[19] = (x[i] >> (19));
        results[20] = (x[i] >> (20));
        results[21] = (x[i] >> (21));
        results[22] = (x[i] >> (22));
        results[23] = (x[i] >> (23));
        results[24] = (x[i] >> (24));
        results[25] = (x[i] >> (25));
        results[26] = (x[i] >> (26));
        results[27] = (x[i] >> (27));
        results[28] = (x[i] >> (28));
        results[29] = (x[i] >> (29));
        results[30] = (x[i] >> (30));
        results[31] = (x[i] >> (31));
        results[32] = (x[i] >> (32));
        results[33] = (x[i] >> (33));
        results[34] = (x[i] >> (34));
        results[35] = (x[i] >> (35));
        results[36] = (x[i] >> (36));
        results[37] = (x[i] >> (37));
        results[38] = (x[i] >> (38));
        results[39] = (x[i] >> (39));
        results[40] = (x[i] >> (40));
        results[41] = (x[i] >> (41));
        results[42] = (x[i] >> (42));
        results[43] = (x[i] >> (43));
        results[44] = (x[i] >> (44));
        results[45] = (x[i] >> (45));
        results[46] = (x[i] >> (46));
        results[47] = (x[i] >> (47));
        results[48] = (x[i] >> (48));
        results[49] = (x[i] >> (49));
        results[50] = (x[i] >> (50));
        results[51] = (x[i] >> (51));
        results[52] = (x[i] >> (52));
        results[53] = (x[i] >> (53));
        results[54] = (x[i] >> (54));
        results[55] = (x[i] >> (55));
        results[56] = (x[i] >> (56));
        results[57] = (x[i] >> (57));
        results[58] = (x[i] >> (58));
        results[59] = (x[i] >> (59));
        results[60] = (x[i] >> (60));
        results[61] = (x[i] >> (61));
        results[62] = (x[i] >> (62));
        results[63] = (x[i] >> (63));
        results[64] = (x[i] >> (64));
        results[65] = (x[i] >> (65));
        results[66] = (x[i] >> (66));
        results[67] = (x[i] >> (67));
        results[68] = (x[i] >> (68));
        results[69] = (x[i] >> (69));
        results[70] = (x[i] >> (70));
        results[71] = (x[i] >> (71));
        results[72] = (x[i] >> (72));
        results[73] = (x[i] >> (73));
        results[74] = (x[i] >> (74));
        results[75] = (x[i] >> (75));
        results[76] = (x[i] >> (76));
        results[77] = (x[i] >> (77));
        results[78] = (x[i] >> (78));
        results[79] = (x[i] >> (79));
        results[80] = (x[i] >> (80));
        results[81] = (x[i] >> (81));
        results[82] = (x[i] >> (82));
        results[83] = (x[i] >> (83));
        results[84] = (x[i] >> (84));
        results[85] = (x[i] >> (85));
        results[86] = (x[i] >> (86));
        results[87] = (x[i] >> (87));
        results[88] = (x[i] >> (88));
        results[89] = (x[i] >> (89));
        results[90] = (x[i] >> (90));
        results[91] = (x[i] >> (91));
        results[92] = (x[i] >> (92));
        results[93] = (x[i] >> (93));
        results[94] = (x[i] >> (94));
        results[95] = (x[i] >> (95));
        results[96] = (x[i] >> (96));
        results[97] = (x[i] >> (97));
        results[98] = (x[i] >> (98));
        results[99] = (x[i] >> (99));
        results[100] = (x[i] >> (100));
        results[101] = (x[i] >> (101));
        results[102] = (x[i] >> (102));
        results[103] = (x[i] >> (103));
        results[104] = (x[i] >> (104));
        results[105] = (x[i] >> (105));
        results[106] = (x[i] >> (106));
        results[107] = (x[i] >> (107));
        results[108] = (x[i] >> (108));
        results[109] = (x[i] >> (109));
        results[110] = (x[i] >> (110));
        results[111] = (x[i] >> (111));
        results[112] = (x[i] >> (112));
        results[113] = (x[i] >> (113));
        results[114] = (x[i] >> (114));
        results[115] = (x[i] >> (115));
        results[116] = (x[i] >> (116));
        results[117] = (x[i] >> (117));
        results[118] = (x[i] >> (118));
        results[119] = (x[i] >> (119));
        results[120] = (x[i] >> (120));
        results[121] = (x[i] >> (121));
        results[122] = (x[i] >> (122));
        results[123] = (x[i] >> (123));
        results[124] = (x[i] >> (124));
        results[125] = (x[i] >> (125));
        results[126] = (x[i] >> (126));
        results[127] = (x[i] >> (127));
        for (j = 0; j < sizeof(results)/sizeof(results[0]); j++) {
            if (results[j] != (x[i] >> (j))) {
                errors++;
            }
        }
    }
    return errors;
}

int main()
{
    int errors = 0;
    errors += test_16bit_lsr ();
    if (errors != 0) {
        abort ();
    }
    return 0;
}
