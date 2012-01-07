//Demo: compute the number of ways that `n` can be represented as sum of 3's, 5's and 7's

int sum_of_3_5_7_helper(int n; int use3; int use5; int use7){

    let (int result = 0;){
        if(use7 == 1){
            if(n >= 7){
                result = result + sum_of_3_5_7_helper(n - 7, 1, 1, 1);
            }
        }
        if(use5 == 1){
            if(n >= 5){
                result = result + sum_of_3_5_7_helper(n - 5, 1, 1, 0);
            }
        }

        if(use3 == 1){
            if(n >= 3){
                result = result + sum_of_3_5_7_helper(n - 3, 1, 0, 0);
            }
        }

        if(n == 0){
            result = 1;
        }

        if(n < 0){
            result = 0;
        }

        return result;
    }
};

int sum_of_3_5_7(int n){
    return sum_of_3_5_7_helper(n, 1, 1, 1);
};

