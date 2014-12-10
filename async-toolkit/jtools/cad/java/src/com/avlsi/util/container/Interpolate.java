package com.avlsi.util.container;

public class Interpolate {
    public static double bilinearInterpolate(final LinearInterpolatable data,
                                             final double x,
                                             final double y) {
        final int indices[] = { data.getBracket(0, x), data.getBracket(1, y) };
        if (indices[0] < 0 || indices[1] < 0) {
            throw new ExtropolateException("data (" + x + ", " + y + ") is " +
                                           "outside the range of the table");
        }

        final double x1 = data.getEntry(0, indices[0]);
        final double y1 = data.getEntry(1, indices[1]);

        final double Q11 = data.getEntry(indices);
        indices[0]++;
        final double Q21 = data.getEntry(indices);
        indices[1]++;

        final double x2 = data.getEntry(0, indices[0]);
        final double y2 = data.getEntry(1, indices[1]);

        final double Q22 = data.getEntry(indices);
        indices[0]--;
        final double Q12 = data.getEntry(indices);

        final double R1 = Q11 * (x2 - x) / (x2 - x1) +
                          Q21 * (x - x1) / (x2 - x1);
        final double R2 = Q12 * (x2 - x) / (x2 - x1) +
                          Q22 * (x - x1) / (x2 - x1);

        return R1 * (y2 - y) / (y2 - y1) +
               R2 * (y - y1) / (y2 - y1);
    }

    public static class ExtropolateException extends RuntimeException {
        public ExtropolateException(String message) {
            super(message);
        }
    }
}
