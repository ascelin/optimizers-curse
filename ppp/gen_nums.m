function gen_nums(n)

array = rand(n,1);

csvwrite(['random_numbers',num2str(n),'.csv'],array)