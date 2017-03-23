package fitness;

import java.util.Arrays;
import java.util.ArrayList;
import java.util.stream.IntStream;

/**
 * Created by audun on 22.03.17.
 */
public class Fitness {
    
    public static double edgeValue(ArrayList<ArrayList<ArrayList<Integer>>> image, ArrayList<ArrayList<Integer>> segments) {
        double sum = 0.0;
        int width = image.get(0).size();
        int height = image.size()-1;
        

        for(ArrayList<Integer> segment : segments) {
            for(int node : segment) {

                int x = node%width;
                int y = node/width;

                if(x > 0) {
                    if(!segment.contains(y*width + (x-1))){
                        sum += Math.sqrt(Math.pow(image.get(y).get(x-1).get(0) - image.get(y).get(x).get(0),2) + Math.pow(image.get(y).get(x-1).get(1) - image.get(y).get(x).get(1),2)
                                + Math.pow(image.get(y).get(x-1).get(2) - image.get(y).get(x).get(2),2));
                    }
                }
                if(y > 0)
                    if(!segment.contains((y-1)*width + x)){
                        sum += Math.sqrt(Math.pow(image.get(y-1).get(x).get(0) - image.get(y).get(x).get(0),2) + Math.pow(image.get(y-1).get(x).get(1) - image.get(y).get(x).get(1),2)
                                + Math.pow(image.get(y-1).get(x).get(2) - image.get(y).get(x).get(2),2));
                    }
                if(x < width-1) {
                    if(!segment.contains(y*width + (x+1))){
                        sum += Math.sqrt(Math.pow(image.get(y).get(x+1).get(0) - image.get(y).get(x).get(0),2) + Math.pow(image.get(y).get(x+1).get(1) - image.get(y).get(x).get(1),2)
                            + Math.pow(image.get(y).get(x+1).get(2) - image.get(y).get(x).get(2),2));
                    }
                }
                if(y < height) {
                    if(!segment.contains((y+1)*width + x)){
                        sum += Math.sqrt(Math.pow(image.get(y+1).get(x).get(0) - image.get(y).get(x).get(0),2) + Math.pow(image.get(y+1).get(x).get(1) - image.get(y).get(x).get(1),2)
                                + Math.pow(image.get(y+1).get(x).get(2) - image.get(y).get(x).get(2),2));
                    }
                }
            }
        }

        return -sum;
    }
}
