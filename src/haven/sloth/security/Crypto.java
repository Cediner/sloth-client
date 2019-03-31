package haven.sloth.security;

import com.google.common.flogger.FluentLogger;
import org.bouncycastle.crypto.engines.AESEngine;
import org.bouncycastle.crypto.modes.GCMBlockCipher;
import org.bouncycastle.crypto.params.AEADParameters;
import org.bouncycastle.crypto.params.KeyParameter;


class Crypto {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    private static final int IV_LEN = 16;
    private static final int MAC_SIZE = 16;

    //key => 32 bytes
    //Output Format:
    //  _________________
    // | 16-byte IV      |
    // |-----------------|
    // | encrypted data  |
    // |-----------------|
    // | 16-byte MAC     |
    // |_________________|
    static byte[] encrypt(byte[] plainText, byte[] key) throws Exception {
        //pad the data
        byte[] paddedData = pad(plainText);

        //generate the IV
        RndKeyGen keygen = new RndKeyGen();
        byte[] IV = keygen.genBytes(IV_LEN);

        //Encrypt
        byte[] cipherText = performCrypto(paddedData, key, IV, true);

        //Prepend IV to first IV.length bytes of cipherText
        cipherText = Pad.prepend(cipherText, IV.length);
        System.arraycopy(IV, 0, cipherText, 0, IV.length);

        return cipherText;
    }

    static byte[] decrypt(byte[] data, byte[] key) throws Exception {
        //Get IV from data
        byte[] IV = new byte[IV_LEN];
        System.arraycopy(data, 0, IV, 0, IV.length);

        //Remove IV from data
        byte[] cipherText = new byte[data.length - 16];
        System.arraycopy(data, 16, cipherText, 0, cipherText.length);

        //Decrypt
        byte[] paddedPlainText = performCrypto(cipherText, key, IV, false);

        //depad the data
        return depad(paddedPlainText);
    }

    private static byte[] performCrypto(byte[] data, byte[] key, byte[] IV, boolean encrypt) throws Exception {
        GCMBlockCipher cipher = new GCMBlockCipher(new AESEngine());

        KeyParameter kp = new KeyParameter(key);
        cipher.init(encrypt, new AEADParameters(kp, MAC_SIZE * 8, IV, null));
        //get the length of what the output data should be +/- MAC
        int inputlen = encrypt ? data.length : data.length - MAC_SIZE;
        int outputlen = encrypt ? data.length + MAC_SIZE : data.length - MAC_SIZE;
        byte[] output = new byte[outputlen];

        int len = 0;
        while (len < inputlen) { //encrypt/decrypt each block
            len += cipher.processBytes(data, len, data.length, output, len);
        }

        try {
            //If Encryption, appends our MAC to the last 16 bytes of data
            //If Decryption, generates MAC and verifies vs MAC encoded in data
            cipher.doFinal(output, len);
        } catch (Exception e) {
            logger.atSevere().log("Decryption failed MAC test");
            throw e;
        }

        return output;
    }


    //PKCS7 padding
    private static byte[] pad(byte[] data) {
        final int bsz = 16;
        int blocks = (int) Math.ceil(data.length / bsz) + 1;
        int padding = blocks * bsz - data.length;

        if (padding == 0)
            padding = bsz;

        byte[] ndat = new byte[data.length + padding];
        System.arraycopy(data, 0, ndat, 0, data.length);
        ndat[ndat.length - 1] = (byte) padding;
        return ndat;
    }

    private static byte[] depad(byte[] data) {
        if (data != null) {
            int padding = (data[data.length - 1] & 0xFF);
            if (padding > 16 || padding == 0)
                return null; //Invalid padding -> corrupted data
            byte[] ndat = new byte[data.length - padding];
            System.arraycopy(data, 0, ndat, 0, ndat.length);
            return ndat;
        } else {
            return null; // corrupted data
        }
    }
}
