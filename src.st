{
    var x = 10+50;
    var y = x + 5;
    y = y * 2;

    {
        var x = 5;
        var y = 10;
        var z = 10 + x + y;
        print (x < y);
        {
            x = 5;
            print(x);
        }
        print (x+y+z);
    }
    print(x);
    print(y);
}
