
//---------------------------

//Đổi dấu

var doi_dau = false;

//Số phép tính ưu tiên tìm thấy

var uu_tien = 0;

//Mảng ghi nhớ các số hạng:

var mang_so_index = 0;
var mang_so = new Array();

//Mảng ghi nhớ các phép tính:

var mang_pt_index = 0;
var mang_pt = new Array();

//CÀI ĐẶT SỰ KIỆN -------------------------


function ClickButton(obj) {


    //Object Ket qua:    
    var ketqua =  document.getElementById('ketqua');

    //Chuỗi hiện tại của kết quả:
    var str_ketqua_old = ketqua.value;

    //Phím mới:
    var phim_moi = '';

    //HTML của phím bấm:
    var type = obj.innerHTML;

    //Nhóm số:

    if ( type == '0'
        ||type == '1'
        ||type == '2'
        ||type == '3'
        ||type == '4'
        ||type == '5'
        ||type == '6'
        ||type == '7'
        ||type == '8'
        ||type == '9'
        ||type == '+/-'
        ||type == '.'  
    )  {
        //Đổi dấu:
        if (type = '+/-') {
            if (doi_dau = true) {
                doi_dau = false;
                phim_moi = phim_moi.substring(1);
            } else {
                doi_dau = true;
                phim_moi = '-' + phim_moi;
            }
        }
        //Số:
        else {
            phim_moi += type;  
        }

        //Thay đổi hiển thị:
        ketqua.value = str_ketqua_old + phim_moi;

    }




    }




