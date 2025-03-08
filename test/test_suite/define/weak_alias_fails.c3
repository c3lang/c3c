module std::math::vector;
def Vec2f = float[<2>];

module abc;
import std::math;
def Vec2f = std::math::vector::Vec2f @weak;
fn Vec2f foo(Vec2f a) { return a * 2; }

module gog;
import std::math;
def Vec2f = std::math::vector::Vec2f;

module deef;
import gog;

def Vec2f = gog::Vec2f @weak;
fn Vec2f foo(Vec2f a) { return a * 2; }

module test;
import abc;
import std::math;
fn void test()
{
	Vec2f a;
	abc::foo(a);
}

module test2;
import abc;
import deef;
fn void test2()
{
	Vec2f a; // #error: abc::Vec2f or deef::Vec2f
	abc::foo(a);
}