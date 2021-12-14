using System.Text.RegularExpressions;

namespace day13
{
  public struct Point
  {
    public readonly int Y;
    public readonly int X;

    public Point(int y, int x)
    {
      Y = y;
      X = x;
    }

    public Point Max(Point rhs) => new Point(Math.Max(Y, rhs.Y), Math.Max(X, rhs.X));

    public int Get(int dimension) => dimension == 0 ? Y : X;

    public Point With(int value, int dimension) => dimension == 0 ? new Point(value, X) : new Point(Y, value);

    public Point After(Fold fold) => With(fold.Value - Math.Abs(fold.Value - Get(fold.Dimension)), fold.Dimension);
  }

  public struct Fold
  {
    public readonly int Value;
    public readonly int Dimension;

    public Fold(int value, int dimension)
    {
      Value = value;
      Dimension = dimension;
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

    private static List<Point> ParsePoints(ref IEnumerator<string> lines)
    {
      return lines
        .TakeWhile(l => !string.IsNullOrWhiteSpace(l))
        .Select(l => l.Split(",").Select(int.Parse).ToList())
        .Select(s => new Point(s[1], s[0]))
        .ToList();
    }

    private static List<Fold> ParseFolds(ref IEnumerator<string> lines)
    {
      return lines
        .TakeWhile(l => !string.IsNullOrWhiteSpace(l))
        .Select(l => Regex.Match(l, @"fold along\s+(\w)\s*=\s*(\d+)").Groups)
        .Select(gs => new Fold(int.Parse(gs[2].Value), gs[1].Value == "y" ? 0 : 1))
        .ToList();
    }

    private static bool[,] PlacePoints(IEnumerable<Point> points)
    {
      Point max = points.Aggregate(new Point(0, 0), (m, p) => m.Max(p));
      bool[,] grid = new bool[max.Y + 1, max.X + 1];
      foreach (var point in points)
      {
        grid[point.Y, point.X] = true;
      }
      return grid;
    }

    private static string ToGridString(this bool[,] grid)
    {
      return string.Join("\n", Enumerable.Range(0, grid.GetLength(0))
        .Select(y => string.Join("", Enumerable.Range(0, grid.GetLength(1))
          .Select(x => grid[y, x] ? "#" : " "))));
    }

    static void Main(string[] args)
    {
      IEnumerator<string> input = File.ReadAllLines("resources/input.txt").ToList().GetEnumerator();
      var points = ParsePoints(ref input);
      var folds = ParseFolds(ref input);
      var foldedOnce = PlacePoints(points.Select(p0 => p0.After(folds[0])));
      var foldedFully = PlacePoints(points.Select(p0 => folds.Aggregate(p0, (p, f) => p.After(f))));

      var part1 = foldedOnce.OfType<bool>().Count(x => x);
      Console.WriteLine($"Part 1: {part1}");

      Console.WriteLine(foldedFully.ToGridString());
    }
  }
}
