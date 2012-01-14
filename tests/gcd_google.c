int gcd(int a; int b){
    if(a < b){
        let (int tmp;){
            tmp = a;
            a = b;
            b = tmp;
        }
    }

    if(b == 0)
        return a;
    else
        return gcd(a - b, b);
};

//returns array which ith element equals
//gcd of every number in `array` except it's ith element
*int gcd_google(*int array; int size){
    let(*int result; int i; int sufix_gcd){
        allocate result[size];

        for(i = 0; i < size; i = i+1){
            result[i] = array[i];
            if(i > 0){
                result[i] = gcd(result[i-1], result[i]);
            }
        }

        if(size < 2){
            return result;
        }

        for(i = size - 1; i >= 0; i = i - 1){
            if(i == size - 1){
                sufix_gcd = result[i];
                result[i] = result[i-1];
            }
            else{
                if(i == 0){
                    result[i] = sufix_gcd;
                }
                else{
                    result[i] = gcd(result[i-1], sufix_gcd);
                }

                sufix_gcd = gcd(sufix_gcd, result[i]);
            }
        }

        return result;
    }
};
