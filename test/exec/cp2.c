
static int AAA[] = {
    1, 2, 3, 4
};

int sum;
int main (void)
{
    sum = AAA[0]; //如果当前文件中，AAA是static的，并且没有行为异常的指针
                           //e.g: int *p=100;  则AAA[0] 应该被替换成1
    return 0;
}

