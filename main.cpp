#include "graphics.hpp"
#include <cstdlib>
#include <iostream>
#include <fstream>
#include <vector>
#include <cmath>
#include <windows.h>

using namespace genv;
using namespace std;

const int XX=800;
const int YY=600;
const int MAX = 255;

struct komplex
{
    double cr, ci;

    komplex(double a, double b)
    {
        cr = a; ci = b;
    }

    double re()
    {
        return cr;
    }

    double im()
    {
        return ci;
    }
};

komplex operator*(komplex a, komplex b)
{
    double r = a.cr*b.cr - a.ci*b.ci;
    double i = a.ci*b.cr + a.cr*b.ci;
    return komplex(r,i);
}

komplex operator+(komplex a, komplex b)
{
    a.cr = a.cr+b.cr;
    a.ci = a.ci+b.ci;
    return a;
}

void rajzol(double a, double b, double c)
{
    for (int x=0; x<XX; x++)
    {
        for (int y=0; y<YY; y++)
        {
            double cr = (a/double(XX))*x-a/2 + (b);
            double ci = (a/double(YY))*y-a/2 + (c);
            komplex c(cr, ci);
            komplex z(0,0);
            int t = 0;
            while (z.re()*z.re()+z.im()*z.im() < 4.0 && t < MAX)
            {
                z = z*z+c; ++t;
            }
            t=t*15;
            gout << move_to(x,y) << color(t,t,t) << dot;
        }
    }
    gout << refresh;
}

int main()
{
    gout.open(XX,YY);
    double nx = 4.0;
    double ex = 0.0;
    double ey = 0.0;

    rajzol(nx, ex, ey);

    event ev;
    while(gin >> ev && ev.keycode != key_escape)
    {
        if(ev.button == btn_left)
        {
            nx /= 2;
            ex = (nx*2/double(XX))*ev.pos_x-nx + (ex);
            ey = (nx*2/double(YY))*ev.pos_y-nx + (ey);
            rajzol(nx, ex, ey);
        }
        if(ev.type==ev_key && ev.keycode=='r')
        {
            nx = 4.0;
            ex = 0.0;
            ey = 0.0;
            rajzol(nx, ex, ey);
        }
        if(ev.button == btn_right)
        {
            nx *= 2;
            ex = (nx*2/double(XX))*ev.pos_x-nx + (ex);
            ey = (nx*2/double(YY))*ev.pos_y-nx + (ey);
            rajzol(nx, ex, ey);
        }
        if(ev.type==ev_key && ev.keycode=='r')
        {
            nx = 4.0;
            ex = 0.0;
            ey = 0.0;
            rajzol(nx, ex, ey);
        }
    }
    return 0;
}

