package dev.fwcd.adventofcode2021;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class Day11 {
    private static final int THRESHOLD = 10;
    private static final int PART1_STEPS = 100;

    public static void main(String[] args) throws IOException {
        List<String> lines = new ArrayList<>();
        try (var reader = new BufferedReader(new InputStreamReader(Day11.class.getResourceAsStream("/input.txt")))) {
            String line;
            while ((line = reader.readLine()) != null) {
                lines.add(line);
            }
        }

        int[][] grid = lines.stream()
            .map(it -> it.chars().map(c -> Character.digit((char) c, 10)).toArray())
            .toArray(int[][]::new);

        int flashes = 0;
        int firstFullFlash = -1;

        for (int i = 0; firstFullFlash < 0 || i < PART1_STEPS; i++) {
            int delta = step(grid);
            if (i < PART1_STEPS) {
                flashes += delta;
            }
            if (delta == (grid.length * grid[0].length)) {
                firstFullFlash = i + 1;
            }
        }

        System.out.println("Part 1: " + flashes);
        System.out.println("Part 2: " + firstFullFlash);
    }

    private static void increaseEnergy(int[][] grid, int y, int x) {
        grid[y][x]++;
        if (grid[y][x] == THRESHOLD) {
            for (int dy = -1; dy <= 1; dy++) {
                for (int dx = -1; dx <= 1; dx++) {
                    int ny = y + dy;
                    int nx = x + dx;
                    if ((dy != 0 || dx != 0) && ny >= 0 && ny < grid.length && nx >= 0 && nx < grid[y].length) {
                        increaseEnergy(grid, ny, nx);
                    }
                }
            }
        }
    }

    private static int step(int[][] grid) {
        int flashes = 0;

        for (int y = 0; y < grid.length; y++) {
            for (int x = 0; x < grid[y].length; x++) {
                increaseEnergy(grid, y, x);
            }
        }

        for (int y = 0; y < grid.length; y++) {
            for (int x = 0; x < grid[y].length; x++) {
                if (grid[y][x] >= THRESHOLD) {
                    grid[y][x] = 0;
                    flashes++;
                }
            }
        }

        return flashes;
    }

    private static String gridToString(int[][] grid) {
        return Arrays.stream(grid)
            .map(row -> Arrays.stream(row).mapToObj(i -> Integer.toString(i)).collect(Collectors.joining()))
            .collect(Collectors.joining("\n"));
    }
}
