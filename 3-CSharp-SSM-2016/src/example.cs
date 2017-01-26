class Hello
{
    int g;
    int a;
    
    void main()
    {
        int b;
        int c;
        b=4;
        c=5;
        if(b==4){
            int a;
            a=6;
            b=square(3);
        }else{
            int d;
            d=8;
        }
    }
    
    int square( int x)
    {
        int b;
        b=x*x;
        return b;   
    }
    int cube( int x)
    {
        return x*x*x;
    }

}
