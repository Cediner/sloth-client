package haven.sloth.script.scripts.api;

import java.util.function.Supplier;

public class Core {
    public static boolean waitUntil(final Supplier<Boolean> state,
                                    final int refresh, final int timeout) throws InterruptedException {
        final long start = System.currentTimeMillis();

        while (!state.get()) {
            Thread.sleep(refresh);
            if (timeout > 0 && (System.currentTimeMillis() - start) >= timeout) {
                return false;
            }
        }

        return true;
    }

    public static boolean waitUntil(final Supplier<Boolean> state, final int timeout) throws InterruptedException {
        return waitUntil(state, 100, timeout);
    }

    public static boolean waitUntil(final Supplier<Boolean> state) throws InterruptedException {
        return waitUntil(state, 100, -1);
    }
}
