package haven.sloth.io;

import com.google.common.flogger.FluentLogger;
import haven.ResCache;

import java.io.*;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;

public class SQLResCache implements ResCache {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    private static final Storage resdb = Storage.create("jdbc:sqlite:data/res.db")
            .orElseThrow(() -> new RuntimeException("Failed to open map database"));
    private static PreparedStatement upsert;
    private static PreparedStatement fetch;

    static {
        resdb.ensure(sql -> {
            try (final Statement stmt = sql.createStatement()) {
                stmt.executeUpdate("CREATE TABLE IF NOT EXISTS resource (\n" +
                        "    name TEXT PRIMARY KEY,\n" +
                        "    data BLOB NOT NULL\n" +
                        ")");
            }
        });

        upsert = resdb.ensurePrepare("INSERT INTO resource (name, data)\n" +
                "\tVALUES (?, ?)\n" +
                "\tON CONFLICT (name) DO UPDATE SET\n" +
                "\t\tdata=excluded.data");
        fetch = resdb.ensurePrepare("SELECT data FROM resource WHERE name =  ?");
    }

    @Override
    public OutputStream store(final String name) {
        return new ByteArrayOutputStream() {
            @Override
            public void close() throws IOException {
                super.close();
                final byte[] data = toByteArray();
                resdb.write(sql -> {
                    upsert.setString(1, name);
                    upsert.setBytes(2, data);
                    upsert.execute();
                });
            }
        };
    }

    @Override
    public synchronized InputStream fetch(final String name) throws IOException {
        final byte[] data;

        try {
            fetch.setString(1, name);
            try (final ResultSet res = fetch.executeQuery()) {
                if (res.next()) {
                    data = res.getBytes(1);
                } else {
                    data = null;
                }
            }
        } catch (SQLException se) {
            logger.atSevere().withCause(se).log("Failed to load resource [%s]", name);
            throw new IOException("Failed to load resource from resdb", se);
        }

        if (data != null) {
            return new ByteArrayInputStream(data);
        } else {
            return null;
        }
    }
}
