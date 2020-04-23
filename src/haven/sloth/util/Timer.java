package haven.sloth.util;

import java.util.ArrayList;
import java.util.List;

public class Timer {
    private class Pair {
        final String tag;
        final long duration;

        public Pair(final String tag, final long dur) {
            this.tag = tag;
            this.duration = dur;
        }
    }

    private final List<Pair> ticks = new ArrayList<>();
    private long start;
    private long last;

    public Timer() {
    }

    public void start() {
        ticks.clear();
        last = System.currentTimeMillis();
        start = last;
    }

    public void tick(final String tag) {
        final long now = System.currentTimeMillis();
        ticks.add(new Pair(tag, now - last));
        last = System.currentTimeMillis();
    }

    public long get(final String tag) {
        for (final Pair p : ticks) {
            if (p.tag == tag) {
                return p.duration;
            }
        }
        return 0;
    }

    public long total() {
        return last - start;
    }

    public String summary() {
        final StringBuffer sb = new StringBuffer();
        ticks.forEach(tick -> {
            sb.append(tick.tag);
            sb.append(": ");
            sb.append(tick.duration);
            sb.append('\n');
        });
        return sb.toString();
    }
}
