/*
 * PNMViewer: a simple viewer for Portable Anymap files
 *
 * Doesn't recognize "raw" formats (P4, P5, P6)
 * Maximum depth for grey and RGB is 255
 *
 * Jeremy Gibbons, July 2004
 *
 */

import java.util.Vector;
import java.io.*;
import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;
import javax.swing.*;

public class PNMViewer extends JComponent {

    BufferedImage image;

    public PNMViewer(String s, int width, int height, FileTokenizer ft) {
        int size = width*height;
        int[] data = new int[size];
        int i = 0;
        if (s.equals("P1")) {
            System.out.println("Bitmap");
            while (i<size && ft.hasMoreTokens()) {
                int b = Integer.parseInt(ft.nextToken());
                if (!(b==0 || b==1)) {
                    System.out.println("Illegal bit value " + b);
                    b=0;
                }
                data[i++]=(1-b) * 0xFFFFFF; 
            }
        } else if (s.equals("P2")) {
            System.out.println("Greymap");
            int max = Integer.parseInt(ft.nextToken());
            if (!(max>0 && max<=255)) {
                System.out.println("Illegal depth " + max);
                max=255;
            }
            int step = 255 / max;
            int greystep = (((step << 8) | step) << 8) | step;
            System.out.println("Depth " + max);
            while (i<size && ft.hasMoreTokens()) {
                int g = Integer.parseInt(ft.nextToken());
                if (!(g>=0 && g<=max)) {
                    System.out.println("Illegal grey value " + g);
                    g=0;
                }
                data[i++]=(max-g) * greystep; 
            }
        } else if (s.equals("P3")) {
            System.out.println("Pixmap");
            int max = Integer.parseInt(ft.nextToken());
            if (!(max>0 && max<=255)) {
                System.out.println("Illegal depth " + max);
                max=255;
            }
            int step = 255 / max;
            System.out.println("Depth " + max);
            while (i<size && ft.hasMoreTokens()) {
                int r = Integer.parseInt(ft.nextToken());
                if (!(r>=0 && r<=max)) {
                    System.out.println("Illegal colour value " + r);
                    r=0;
                }
                int g = Integer.parseInt(ft.nextToken());
                if (!(g>=0 && g<=max)) {
                    System.out.println("Illegal colour value " + g);
                    g=0;
                }
                int b = Integer.parseInt(ft.nextToken());
                if (!(b>=0 && b<=max)) {
                    System.out.println("Illegal colour value " + b);
                    b=0;
                }
                data[i++] = ((((r*step) << 8) | (g*step)) << 8) | (b*step); 
            }
        } else {
            System.out.println("Unrecognized magic string " + s);
            System.exit(1);
        }
        if (ft.hasMoreTokens())
            System.out.println("Unexpected additional data ignored.");
        if (i<size)
            System.out.println("Unexpected end of file.");
        image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
        image.setRGB(0, 0, width, height, data, 0, width);
    }

    public void paint(Graphics g) {
        g.drawImage(image, 0, 0, this);
    }
    
    public static void main (String[] args) {

        String magic=null;
        int w=0, h=0;
        if (args.length==1) {
            FileTokenizer ft = new FileTokenizer(args[0]);
            if (ft.hasMoreTokens()) 
                magic = ft.nextToken();
            else {
                System.out.println("Unexpected end of file.");
                System.exit(1);
            }
            if (ft.hasMoreTokens()) {
                w = Integer.parseInt(ft.nextToken());
                if (w<=0) {
                    System.out.println("Width " + w + ": should be positive");
                    System.exit(1);
                }
            } else {
                System.out.println("Unexpected end of file.");
                System.exit(1);
            }
            if (ft.hasMoreTokens()) {
                h = Integer.parseInt(ft.nextToken());
                if (h<=0) {
                    System.out.println("Height " + h + ": should be positive");
                    System.exit(1);
                }
            } else {
                System.out.println("Unexpected end of file.");
                System.exit(1);
            }
            System.out.println("Image size " + w + "*" + h);
            JFrame f = new JFrame("PNMViewer");
            f.getContentPane().add(new PNMViewer(magic,w,h,ft));
            f.setSize(w+11,h+29);
            f.setLocation(100,100);
            f.addWindowListener(new WindowAdapter() {
                    public void windowClosing(WindowEvent e) {
                        System.exit(0);
                    }
                });
            f.setVisible(true);
        } else {
            System.out.println("PNMViewer, by Jeremy Gibbons, July 2004.");
            System.out.println("Usage: java PNMViewer pic.pnm");
        }
    }
}

class FileTokenizer {
    private File file; 
    private BufferedReader in;
    private Vector tokens = new Vector();
    private void getTokens() {
        try {
            String line;
            do {
                line = in.readLine();
                if (line != null) {
                    String[] words = line.split("\\s|\\t");
                    for (int i=0; i<words.length; i++)
                        if (words[i].length()>0)
                            tokens.addElement(words[i]);
                }
            } while ((tokens.isEmpty()) && (line != null));
        } catch (IOException e) {
            System.out.println("IO exception: " + e);
            System.exit(1);
        }
    }

    public FileTokenizer(String filename) {
        file = new File(filename);
        if (!file.exists() || !file.canRead()) {
            System.out.println("Can't read " + file);
            System.exit(1);
        }
        try {
            in = new BufferedReader(new FileReader(file));
            getTokens();
        } catch (FileNotFoundException e) {
            System.out.println("File disappeared.");
            System.exit(1);
        }
    }
    public String nextToken() {
        if (hasMoreTokens()) {
            String token = (String)tokens.firstElement();
            tokens.removeElementAt(0);
            return token;
        }
        else return null;
    }
    public boolean hasMoreTokens() {
        if (tokens.isEmpty()) getTokens();
        return !tokens.isEmpty();
    }
}
