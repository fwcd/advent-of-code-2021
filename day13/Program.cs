﻿using System.Text.RegularExpressions;

namespace day13
{
  public struct Point
  {
    public readonly int X;
    public readonly int Y;

    public Point(int x, int y)
    {
      X = x;
      Y = y;
    }
  }

  public struct Fold
  {
    public readonly int Value;
    public readonly bool AlongX;

    public Fold(int value, bool alongX)
    {
      Value = value;
      AlongX = alongX;
    }
  }

  static class Program
  {
    private static List<T> TakeWhile<T>(this IEnumerator<T> enumerator, Predicate<T> p)
    {
        var elems = new List<T>();
        while (enumerator.MoveNext() && p(enumerator.Current))
        {
            elems.Add(enumerator.Current);
        }
        return elems;
    }

    private static void Skip<T>(this IEnumerator<T> enumerator, int n)
    {
        for (int i = 0; i < n; i++)
        {
            enumerator.MoveNext();
        }
    }

    private static List<Point> ParsePoints(ref IEnumerator<string> lines)
    {
      return lines
        .TakeWhile(l => !string.IsNullOrWhiteSpace(l))
        .Select(l => l.Split(",").Select(int.Parse).ToList())
        .Select(s => new Point(s[0], s[1]))
        .ToList();
    }

    private static List<Fold> ParseFolds(ref IEnumerator<string> lines)
    {
      return lines
        .TakeWhile(l => !string.IsNullOrWhiteSpace(l))
        .Select(l => Regex.Match(l, @"fold along\s+(\w)\s*=\s*(\d+)").Groups)
        .Select(gs => new Fold(int.Parse(gs[2].Value), gs[1].Value == "x"))
        .ToList();
    }

    static void Main(string[] args)
    {
      IEnumerator<string> input = File.ReadAllLines("resources/demo.txt").ToList().GetEnumerator();
      var points = ParsePoints(ref input);
      var folds = ParseFolds(ref input);

      Console.WriteLine(string.Join(", ", points.Select(p => $"({p.X}, {p.Y})")));
      Console.WriteLine(string.Join(", ", folds.Select(f => $"(fold along {(f.AlongX ? 'x' : 'y')}={f.Value})")));
    }
  }
}
