//#include <stdio.h>
//#include <stdlib.h>

int rand(void);
int getchar(void);
int system(const char *command);
int printf(const char *,...);
int exit(int);

int width = 20, height = 20;
int x, y, fruitX, fruitY, score;
int tailX[100], tailY[100];
int nTail;
enum eDirecton { STOP = 0, LEFT, RIGHT, UP, DOWN};
enum eDirecton dir;

void Setup()
{
    dir = STOP;
    x = width / 2;
    y = height / 2;
    fruitX = rand() % width;
    fruitY = rand() % height;
    score = 0;
}

void Draw()
{
    //system("clear");
    int i;
    for (i = 0; i < width+2; i++)
        printf("#");
    printf("\n");

    for (i = 0; i < height; i++)
    {
        for (int j = 0; j < width; j++)
        {
            if (j == 0)
                printf("#");
            if (i == y && j == x)
                printf("*");
            else if (i == fruitY && j == fruitX)
                printf("%");
            else
            {
                int print = 0;
                for (int k = 0; k < nTail; k++)
                {
                    if (tailX[k] == j && tailY[k] == i)
                    {
                        printf("*"); print = 1;
                    }
                }
                if (!print)
                    printf(" ");

            }

            if (j == width - 1)
                printf("#");
        }
        printf("\n");
    }

    for (int i = 0; i < width+2; i++)
        printf("#");
    printf("\n");
    printf("Score:%d\n", score);
}

void Input()
{
    //HACK
    dir = LEFT;
    return;

    //if (_kbhit())
    {
        switch (getchar())
        {
        case 'a':
            dir = LEFT;
            break;
        case 'd':
            dir = RIGHT;
            break;
        case 'w':
            dir = UP;
            break;
        case 's':
            dir = DOWN;
            break;
        case 'x':
            exit(0);
            break;
        }
    }
}

void algorithm()
{
    int prevX = tailX[0];
    int prevY = tailY[0];
    int prev2X, prev2Y;
    tailX[0] = x;
    tailY[0] = y;

    for(int i = 1; i < nTail ; i++)
    {
        prev2X = tailX[i];
        prev2Y = tailY[i];
        tailX[i] = prevX;
        tailY[i] = prevY;
        prevX = prev2X;
        prevY = prev2Y;
    }

    switch (dir)
    {
    case LEFT:
        x--;
        break;
    case RIGHT:
        x++;
        break;
    case UP:
        y--;
        break;
    case DOWN:
        y++;
        break;
    default:
        break;
    }
    if (x >= width)
        x = 0; else if (x < 0) x = width - 1;
    if (y >= height)
        y = 0; else if (y < 0) y = height - 1;

    for (int i = 0; i < nTail ; i++)
        if (tailX[i] == x && tailY[i] == y)
            exit(0);

    if (x == fruitX && y == fruitY)
    {
        score += 10;
        fruitX = rand() % width;
        fruitY = rand() % height;
        nTail++;
    }
}

void init_global()
{
    width = 20, height = 20;
}

int main()
{
    init_global();
    Setup();
    int n=3;
    while (n > 0)
    //while (1)
    {
        Draw();
        Input();
        algorithm();
        //Sleep(10);
        n--;
    }
    return 0;
}
