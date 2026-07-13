long long ch;
int main()
{
    ch = 'a';
    switch (ch) {
    case '(':
    case ')':
    case ',':
    case '/':
    case ':':
    case ';':
    case '<':
    case '=':
    case '>':
    case '?':
    case '@':
    case '[':
    case '\\':
    case ']':
    case '{':
    case '}':
      return 1;
    }
    return 0;
}
