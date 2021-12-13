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

    private static bool[,] PlacePoints(List<Point> points)
    {
      Point max = points.Aggregate(new Point(0, 0), (m, p) => m.Max(p));
      bool[,] grid = new bool[max.Y + 1, max.X + 1];
      foreach (var point in points)
      {
        grid[point.Y, point.X] = true;
      }
      return grid;
    }

    private static IEnumerable<Point> Points(this bool[,] grid)
    {
      for (int y = 0; y < grid.GetLength(0); y++)
      {
        for (int x = 0; x < grid.GetLength(1); x++)
        {
          yield return new Point(y, x);
        }
      }
    }

    private static bool IsInBounds(this bool[,] grid, Point point) =>
      point.Y >= 0 && point.Y < grid.GetLength(0) && point.X >= 0 && point.X < grid.GetLength(1);

    private static bool Get(this bool[,] grid, Point point) => grid[point.Y, point.X];

    private static void Combine(this bool[,] grid, Point point, bool value) => grid[point.Y, point.X] |= value;

    private static bool[,] ApplyFold(this bool[,] grid, Fold fold)
    {
      int[] newDimensions = Enumerable.Range(0, grid.Rank).Select(grid.GetLength).ToArray();
      newDimensions[fold.Dimension] = fold.Value;

      bool[,] newGrid = new bool[newDimensions[0], newDimensions[1]];
      foreach (var point in newGrid.Points())
      {
        newGrid.Combine(point, grid.Get(point));

        var foldPoint = point.With(2 * fold.Value - point.Get(fold.Dimension), fold.Dimension);
        if (grid.IsInBounds(foldPoint))
        {
          newGrid.Combine(point, grid.Get(foldPoint));
        }
      }
      return newGrid;
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
      var grid = PlacePoints(points);

      var foldedOnce = grid.ApplyFold(folds[0]);
      var folded = folds.Aggregate(grid, ApplyFold);

      var part1 = foldedOnce.OfType<bool>().Count(x => x);
      Console.WriteLine($"Part 1: {part1}");

      var part2 = folded;
      Console.WriteLine(folded.ToGridString());
    }
  }
}
