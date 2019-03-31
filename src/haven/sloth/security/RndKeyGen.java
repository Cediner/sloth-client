package haven.sloth.security;

import org.bouncycastle.crypto.digests.SHA256Digest;
import org.bouncycastle.crypto.macs.HMac;
import org.bouncycastle.crypto.prng.*;

//Used to generate random bytes of various lengths based on keys as seed
class RndKeyGen {
    private SP800SecureRandom rnd;

    RndKeyGen() {
        SP800SecureRandomBuilder rndbuild = new SP800SecureRandomBuilder();
        rnd = rndbuild.buildHMAC(new HMac(new SHA256Digest()), null, false);
    }

    byte[] genBytes(int bytes) {
        byte[] ret = new byte[bytes];
        rnd.nextBytes(ret);
        return ret;
    }
}
