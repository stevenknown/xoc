int a, b, c;
float a2, b2, c2;
double a3, b3, c3;
int i;
bool j;
int main()
{
    c = (b - a) + a;
    c2 = (b2 - a2) + a2;
    c3 = (b3 - a3) + a3;
    c2 = b2 - 0.0;
    c3 = b3 - 0.0;
    c = (b * a) / a;
    c2 = (b2 * a2) / a2;
    c = (b / a) * a;
    c2 = (b2 / a2) * a2;
    c = (i ? b2 : b2 && c3) && c3;
    c = (j ? b2 : b2 || c3) || c3;
    return 0;
}
