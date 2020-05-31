// from NASA GISS temp data, extracts mean 2019 temp per station

import java.io.*;
import java.util.StringTokenizer;

public class Temperature_Cleaner {
    public static void main(String[] args) throws IOException {
        // input
        BufferedReader br = new BufferedReader(new FileReader("/Users/mt/CoronaVision/Data/v4.mean_GISS_homogenized.txt"));
        PrintWriter pw = new PrintWriter(new BufferedWriter(new FileWriter("/Users/mt/CoronaVision/Data/v4.mean_GISS_homogenized.csv")));

        // headers
        pw.println("Station,2019_mean");

        // data
        String line = br.readLine();
        String station = null;
        int stations = 0; // informative cleaning stats
        int present = 0;
        while (line != null) {
            StringTokenizer st = new StringTokenizer(line);
            String token = st.nextToken();
            if (token.length() == 11) { // marks new station
                station = token;
                System.out.println(station);
                stations++;
            } else if (token.equals("2019")) { // marks wanted data, most recent complete year
                double avg = 0;
                boolean missing = false;
                while (st.hasMoreTokens()) {
                    int next = Integer.parseInt(st.nextToken());
                    if (next == -9999) {
                        missing = true;
                        break;
                    }
                    avg += next;
                }
                if (missing) {
                    line = br.readLine();
                    continue;
                }
                System.out.println("2019 present");
                present++;
                avg /= 1200; // 12 months, data is formatted without decimal point (e.g. 426 is actually 4.26 Celsius)
                pw.println(station + "," + avg);
            }
            line = br.readLine();
        }

        System.out.println(present + " stations with full 2019 data out of " + stations + " stations, or about " +
                Math.round((double) present / stations * 100) + " percent");
        pw.close();
    }
}
