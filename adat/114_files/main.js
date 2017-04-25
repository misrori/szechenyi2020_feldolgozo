$(function () {
    wait(1);
    manButtons();
    writeRefreshDate();
    getCount();
    op_type = $('input[name=op_type]:checked').val();
    option_first = '<option value="">==== ' + nyelv['minden'] + ' ====</option>';
    getSelect('forras_uj');
    getSelect('kiiras_eve');
    getSelect(op_type);
    getSelect('eupik_nev');
    allPlace = '';
    if (pre_eupik_nev != undefined && pre_eupik_nev != null && parseInt(pre_eupik_nev) > 0)
    {
        $('#forras').val(pre_forras);
        getSelect('op_nev');
        $('#op_nev').val(pre_op_nev);
        getSelect('eupik_nev');
        $('#eupik_nev').val(pre_eupik_nev);
        $('#tab1').removeClass('active');
        $('#tab2').addClass('active');
        manButtons();
        $('#btn_search').trigger('click');
    }
    getPlace('all', '', 'helyseg');
    wait();
    $('#regio').change(function () {
        hide(1);
        if ($('#regio').val() == 0) {
            $('#ttype').val('');
            $('#ttipus').val('orszag');
            fillSelectPlace(allPlace, '', '');
        } else {
            $('#ttype').val('regio');
            $('#ttipus').val('regio');
            $('#tkod').val($('#regio').val());
            fillSelectPlace(allPlace, $('#regio').val(), 'regio');
        }
        //getCount();
        return true;
    });
    wait();
    $('#megye').change(function () {
        hide(1);
        if ($('#megye').val() == 0) {
            if ($('#regio').val() == 0) {
                $('#ttipus').val('orszag');
                $('#tkod').val('');
            } else {
                $('#ttipus').val('regio');
                $('#tkod').val($('#regio').val());
            }
            fillSelectPlace(allPlace, $('#regio').val(), 'regio');
        } else {
            $('#ttype').val('megye');
            $('#ttipus').val('megye');
            $('#tkod').val($('#megye').val());
            fillSelectPlace(allPlace, $('#megye').val(), 'megye');
        }
        //getCount();
        return true;
    });
    wait();
    $('#kisterseg').change(function () {
        hide(1);
        if ($('#kisterseg').val() == 0) {
            if ($('#megye').val() == 0) {
                if ($('#regio').val() == 0) {
                    $('#ttipus').val('orszag');
                    $('#tkod').val('');
                } else {
                    $('#ttipus').val('regio');
                    $('#tkod').val($('#regio').val());
                }
            } else {
                $('#ttipus').val('megye');
                $('#tkod').val($('#megye').val());
            }
            fillSelectPlace(allPlace, $('#megye').val(), 'megye');
        } else {
            $('#ttype').val('kisterseg');
            $('#ttipus').val('kisterseg');
            $('#tkod').val($('#kisterseg').val());
            fillSelectPlace(allPlace, $('#kisterseg').val(), 'kisterseg');
        }
        //getCount();
        return true;
    });
    wait();
    $('#helyseg').change(function () {
        hide(1);
        if ($('#helyseg').val() == 0) {
            if ($('#kisterseg').val() == 0) {
                if ($('#megye').val() == 0) {
                    if ($('#regio').val() == 0) {
                        $('#ttipus').val('orszag');
                        $('#tkod').val('');
                    } else {
                        $('#ttipus').val('regio');
                        $('#tkod').val($('#regio').val());
                    }
                } else {
                    $('#ttipus').val('megye');
                    $('#tkod').val($('#megye').val());
                }
            } else {
                $('#ttipus').val('kisterseg');
                $('#tkod').val($('#kisterseg').val());
            }
            fillSelectPlace(allPlace, $('#kisterseg').val(), 'kisterseg');
        } else {
            $('#ttype').val('helyseg');
            $('#ttipus').val('telepules');
            $('#tkod').val($('#helyseg').val());
            fillSelectPlace(allPlace, $('#helyseg').val(), 'helyseg');
        }
        //getCount();
        return true;
    });
    wait();
    $('#forras').change(function () {
        hide(1);
        var forras = $('#forras').val();
        var op_types_div = $('#op_types_div');
        var div_op_nev = $('#div_op_nev');
        var div_kitoresi_pont = $('#div_kitoresi_pont');
        // eupik_nev
        var emp = [];
        fillSelect('eupik_nev', emp);
        // forras_uj
        getSelect('forras_uj');
        // kiiras_eve
        getSelect('kiiras_eve');
        // op_nev | kitoresi_pont
        if (forras == 'uszt') {
            op_types_div.show();
        } else {
            op_types_div.hide();
            div_kitoresi_pont.hide();
            div_op_nev.show();
            op_type = 'op_nev';
        }
        getSelect(op_type);
        var label_op_nev = nyelv['operativ_program'];
        var label_eupik_nev = nyelv['tamogatasi_konstrukcio'];
        if (forras == 'ktia')
        {
            var label_op_nev = nyelv['programcsoport'];
            var label_eupik_nev = nyelv['program'];
        }

        $('#label_op_nev').html(label_op_nev + ':');
        $('#label_eupik_nev').html(label_eupik_nev + ':');
        // eupik_nev
        getSelect('eupik_nev');
        getCount();
        return true;
    });
    $('#forras_uj').change(function () {
        hide(1);
        getCount();
        return true;
    });
    $('#palyazo_nev').change(function () {
        hide(1);
        getCount();
        return true;
    });
    $('#kiiras_eve').change(function () {
        hide(1);
        getCount();
        return true;
    });
    $('#op_nev').change(function () {
        hide(1);
        getSelect('eupik_nev');
        getCount();
        return true;
    });
    $('#kitoresi_pont').change(function () {
        hide(1);
        getSelect('eupik_nev');
        getCount();
        return true;
    });
    $('#eupik_nev').change(function () {
        hide(1);
        getCount();
        return true;
    });
    $('input[name=op_type]').change(function () {
        op_type = $('input[name=op_type]:checked').val();
        div_on = $('#div_op_nev');
        div_kp = $('#div_kitoresi_pont');
        on = $('#op_nev');
        kp = $('#kitoresi_pont');
        on.html('').prop('disabled', false);
        kp.html('').prop('disabled', false);
        div_on.hide();
        div_kp.hide();
        if (op_type == 'op_nev') {
            div_on.show();
        } else if (op_type == 'kitoresi_pont') {
            div_kp.show();
        }

        getSelect(op_type);
        getSelect('eupik_nev');
        return true;
    });
    $('#btn_next').click(function () {
        $('#tab1').removeClass('active');
        $('#tab2').addClass('active');
        manButtons();
        return true;
    });
    $('#btn_prev').click(function () {
        $('#tab2').removeClass('active');
        $('#tab1').addClass('active');
        manButtons();
        return true;
    });
    $('html').click(function () {
        $('.icon-print').popover('hide');
    });
    $('.icon-print').popover().click(function (e) {
        $('#print_all, #print_page').click(function () {
            $('#print').val('1');
            var params = $('#searchform').serialize();
            if ($(this).attr('id') == 'print_all') {
                params += '&print_all=1';
            }
            if ($('.ui-sort-ltr').not('[class~="ui-state-disabled"]').attr('sort') !== undefined) {
                params += '&sort=' + $('.ui-sort-ltr').not('[class~="ui-state-disabled"]').attr('sort');
                params += '&order=' + $('.ui-sort-ltr').not('[class~="ui-state-disabled"]').parent().parent().attr('id').split('jqgh_grid_')[1];
                params += '&page=' + $('.ui-pg-input').val();
                params += '&rows=' + $('.ui-pg-selbox').val();
            }
            var wparams = 'menubar=yes, scrollbars=yes, resizeable=yes, fullscreen=yes';
            var url = 'index.php?node=print&' + params;
            window.open(url, '_blank', wparams);
            $('.icon-print').popover('hide');
        });
        e.stopPropagation();
    });
    //redmine_16928
    $('#div_export_button').click(function (e) {
        if ($('#btn_search').prop('disabled') == false) {
            $('#export').val('1');
            var params = $('#searchform').serialize();
            if ($('.ui-sort-ltr').not('[class~="ui-state-disabled"]').attr('sort') !== undefined) {
                params += '&sort=' + $('.ui-sort-ltr').not('[class~="ui-state-disabled"]').attr('sort');
                params += '&order=' + $('.ui-sort-ltr').not('[class~="ui-state-disabled"]').parent().parent().attr('id').split('jqgh_grid_')[1];
                params += '&page=' + $('.ui-pg-input').val();
                params += '&rows=' + $('.ui-pg-selbox').val();
            }
            var wparams = 'menubar=yes, scrollbars=yes, resizeable=yes, fullscreen=yes';
            var url = 'index.php?node=export&' + params;
            document.location.href = url;
            e.stopPropagation();
        }
    });
    $('#div_help_button').click(function () {
        location.replace('files/help/nyertes.pdf');
    });
});
function manButtons() {
    if ($('#tab1').hasClass('active')) {
        $('#btn_next').prop('disabled', false);
        $('#btn_prev').prop('disabled', true);
        $('#btn_search').prop('disabled', true);
        $('#li_tab1').css('font-weight', 'bold');
        $('#li_tab2').css('font-weight', 'normal');
    } else {
        $('#btn_next').prop('disabled', true);
        $('#btn_prev').prop('disabled', false);
        $('#btn_search').prop('disabled', false);
        $('#li_tab2').css('font-weight', 'bold');
        $('#li_tab1').css('font-weight', 'normal');
    }

    return true;
}

function getSelect(name) {
    wait(1);
    var forras = $('#forras').val();
    switch (name) {
        case 'forras_uj':
            var prm = {'name': name, 'forras': forras};
            break;
        case 'kiiras_eve':
            var prm = {'name': name, 'forras': forras};
            break;
        case 'op_nev':
            var prm = {'name': name, 'forras': forras};
            break;
        case 'kitoresi_pont':
            var prm = {'name': name, 'forras': forras};
            break;
        case 'eupik_nev':
            var forras = $('#forras').val();
            var op_type = $('input[name=op_type]:checked').val();
            var op_nev = '';
            var kitoresi_pont = '';
            var kiiras_eve = '';
            if (forras == 'uszt') {
                if (op_type == 'op_nev') {
                    op_nev = $('#op_nev').val();
                } else if (op_nev == 'kitoresi_pont') {
                    kitoresi_pont = $('#kitoresi_pont').val();
                }
            } else {
                op_type = 'op_nev';
            }

            var prm = {
                'name': name,
                'forras': forras,
                'op_type': op_type,
                'op_nev': $('#op_nev').val(),
                'kitoresi_pont': $('#kitoresi_pont').val(),
                'kiiras_eve': kiiras_eve
            };
            break;
        default:
            break;
    }

    var url = 'index.php?node=get_select';
    var data = $.param(prm);
    $.ajax({
        url: url,
        async: false,
        type: 'get',
        dataType: 'json',
        data: data,
        success: function (jsn) {
            if (jsn.length <= 0) {
                fillSelect(name, jsn);
                $('#' + name).prop('disabled', true);
            } else {
                fillSelect(name, jsn);
                $('#' + name).prop('disabled', false);
            }
        }
    });
    wait(0);
    return true;
}

function fillSelect(name, jsn) {
    out = '<option value="">==== ' + nyelv['minden'] + ' ====</option>';
    $.each(jsn, function (key, val) {
        out += '<option value="' + val.ID + '">' + val.CONTENT + '</option>';
    });
    if ($('#' + name).html(out)) {
        getCount();
        return true;
    } else {
        getCount();
        return false;
    }
}

function adatlap(forras, id)
{
    var tart = $('#adatlap_tartalom');
    tart.html('');
    var url = 'index.php?node=details';
    var params = {
        'forras': forras,
        'id': id
    };
    $.ajax({
        url: url,
        async: false,
        type: 'get',
        data: params,
        dataType: 'html',
        beforeRequest: function () {
            $.blockUI({message: '<img src="assets/img/ajax-loader.gif" />'});
        },
        success: function (data) {
            tart.html(data);
            // start btns
            var url = 'index.php?node=tamproj';
            var params = {'id': id, 'forras': forras, 'num': 1};
            $.ajax({
                url: url,
                async: false,
                type: 'get',
                data: params,
                dataType: 'html',
                success: function (data_1) {
                    $('#collapse_1_content').html(data_1);
                }
            });
            var params = {'id': id, 'forras': forras, 'num': 2};
            $.ajax({
                url: url,
                async: false,
                type: 'get',
                data: params,
                dataType: 'html',
                success: function (data_2) {
                    $('#collapse_2_content').html(data_2);
                }
            });
            var params = {'id': id, 'forras': forras, 'num': 3};
            $.ajax({
                url: url,
                async: false,
                type: 'get',
                data: params,
                dataType: 'html',
                success: function (data_3) {
                    $('#collapse_3_content').html(data_3);
                }
            });
            var params = {'id': id, 'forras': forras, 'num': 4};
            $.ajax({
                url: url,
                async: false,
                type: 'get',
                data: params,
                dataType: 'html',
                success: function (data_4) {
                    $('#collapse_4_content').html(data_4);
                }
            });
			var params = {'id': id, 'forras': forras, 'num': 5};
			$.ajax({
				url: url,
				async: false,
				type: 'get',
				data: params,
				dataType: 'html',
				success: function(data_5) {
					$('#collapse_5_content').html(data_5);
				}
			});

            // end btns
        }
    });
    return true;
}

function writeRefreshDate() {
    var url = 'index.php?node=refresh_date';
    $.ajax({
        url: url,
        async: false,
        type: 'GET',
        dataType: 'html',
        success: function (data) {
            $('#refresh_date').html(data);
        }
    });
    return true;
}

function getCount() {
    var url = 'index.php?node=count';
    var params = $('#searchform').serialize();
    //alert(params);
    $.ajax({
        url: url,
        async: false,
        type: 'POST',
        data: params,
        dataType: 'html',
        success: function (data) {
            //alert(data);
            $('#result_counter').html(data);
        }
    });
    return true;
}

function wait(mode) {
    if (mode == undefined || mode == null || mode != 1) {
        mode = 0;
    }
    var w = $('#wait');
    if (mode == 1) {
        $('#wait').modal('show');
    } else {
        $('#wait').modal('hide');
    }
    return true;
}

function hide(prm)
{
    if (prm == null || prm == undefined || prm == '' || prm == 0)
    {
        var vis = 'block';
    } else
    {
        var vis = 'none';
    }

    var full = $('#fulltable');
    full.css('display', vis);
}

function openwin(url, ww, wh) {
    var sw = screen.width;
    var sh = screen.height;
    var wleft = (sw / 2) - (ww / 2);
    var wtop = (sh / 2) - (wh / 2);
    var wparams = 'width=' + ww + ',height=' + wh + ',left=' + wleft + ',top=' + wtop + ',scrollbars=yes';
    window.open(url, 'winx', wparams);
}

function getPlace(name, tkod, fill, according) {
    wait(1);
    tkod = typeof tkod !== 'undefined' ? tkod : '';
    fill = typeof fill !== 'undefined' ? fill : '';
    according = typeof according !== 'undefined' ? according : '';
    var kod = '';
    if (tkod > 0) {
        kod = tkod;
    }
    var prm = {
        'name': 'place',
        'sqltype': 'place',
        'tkod': kod,
        'ttype': name
    };
    var url = 'index.php?node=get_select';
    var data = $.param(prm);
    //alert(data);
    $.ajax({
        url: url,
        async: false,
        type: 'get',
        dataType: 'json',
        data: data,
        cache: true,
        success: function (jsn) {
            allPlace = jsn;
            fillSelectPlace(jsn, fill, according);
        }
    });
    return true;
}

function in_array_r(arr, field, value) {
    var ret = false;
    $.each(arr, function (key, val) {
        $.each(val, function (key2, val2) {
            //console.log(" -- key2: " + key2);
            //console.log("field: " + field);
            if (key2 == field) {
                if (val2 == value) {
                    //console.log(" ++ val2: " + val2 + " * " + value);
                    ret = true;
                }
            }
        });
    });
    return ret;
}

function reuse() {
    $('#ttype').val('');
    fillSelectPlace(allPlace, '', '');
}

function fillSelectPlace(jsn, fill, according) {

    according = typeof according !== 'undefined' ? according : 'helyseg';
    var regio_ = $('#regio').val();
    var megye_ = $('#megye').val();
    var kisterseg_ = $('#kisterseg').val();
    var helyseg_ = $('#helyseg').val();
    var regio = [];
    var megye = [];
    var kisterseg = [];
    var helyseg = [];
    /*ha teljesen feltˆltj¸k a mezıket*/
    if ($('#ttype').val() == '') {
        $.each(jsn, function (key, val) {
            helyseg.push({
                HELYSEG_KOD: val.HELYSEG_KOD,
                HELYSEG: val.HELYSEG,
                SORT: asWithoutAccent(val.HELYSEG)
            });
            if (!in_array_r(kisterseg, 'KISTERSEG_KOD', val.KISTERSEG_KOD)) {
                kisterseg.push({
                    KISTERSEG_KOD: val.KISTERSEG_KOD,
                    KISTERSEG: val.KISTERSEG,
                    SORT: asWithoutAccent(val.KISTERSEG)
                });
            }
            if (!in_array_r(megye, 'MEGYE_KOD', val.MEGYE_KOD)) {
                megye.push({
                    MEGYE_KOD: val.MEGYE_KOD,
                    MEGYE: val.MEGYE,
                    SORT: asWithoutAccent(val.MEGYE)
                });
            }
            if (!in_array_r(regio, 'REGIO_KOD', val.REGIO_KOD)) {
                regio.push({
                    REGIO_KOD: val.REGIO_KOD,
                    REGIO: val.REGIO,
                    SORT: asWithoutAccent(val.REGIO)
                });
            }
        });
    }
    if (according == 'regio') {
        $.each(jsn, function (key, val) {
            if (val.REGIO_KOD == fill) {
                helyseg.push({
                    HELYSEG_KOD: val.HELYSEG_KOD,
                    HELYSEG: val.HELYSEG,
                    SORT: asWithoutAccent(val.HELYSEG)
                });
            }
            if (!in_array_r(kisterseg, 'KISTERSEG_KOD', val.KISTERSEG_KOD) && val.REGIO_KOD == fill) {
                kisterseg.push({
                    KISTERSEG_KOD: val.KISTERSEG_KOD,
                    KISTERSEG: val.KISTERSEG,
                    SORT: asWithoutAccent(val.KISTERSEG)
                });
            }
            if (!in_array_r(megye, 'MEGYE_KOD', val.MEGYE_KOD) && val.REGIO_KOD == fill) {
                megye.push({
                    MEGYE_KOD: val.MEGYE_KOD,
                    MEGYE: val.MEGYE,
                    SORT: asWithoutAccent(val.MEGYE)
                });
            }
            if (!in_array_r(regio, 'REGIO_KOD', val.REGIO_KOD)) {
                regio.push({
                    REGIO_KOD: val.REGIO_KOD,
                    REGIO: val.REGIO,
                    SORT: asWithoutAccent(val.REGIO)
                });
            }
        });
    }
    if (according == 'megye') {
        $.each(jsn, function (key, val) {
            if (val.MEGYE_KOD == fill) {
                helyseg.push({
                    HELYSEG_KOD: val.HELYSEG_KOD,
                    HELYSEG: val.HELYSEG,
                    SORT: asWithoutAccent(val.HELYSEG)
                });
            }
            if (!in_array_r(kisterseg, 'KISTERSEG_KOD', val.KISTERSEG_KOD) && val.MEGYE_KOD == fill) {
                kisterseg.push({
                    KISTERSEG_KOD: val.KISTERSEG_KOD,
                    KISTERSEG: val.KISTERSEG,
                    SORT: asWithoutAccent(val.KISTERSEG)
                });
            }
            if (!in_array_r(megye, 'MEGYE_KOD', val.MEGYE_KOD) && val.MEGYE_KOD == $('#megye').val()) {
                regio_ = val.REGIO_KOD;
                megye.push({
                    MEGYE_KOD: val.MEGYE_KOD,
                    MEGYE: val.MEGYE,
                    SORT: asWithoutAccent(val.MEGYE)
                });
            }
            if (!in_array_r(regio, 'REGIO_KOD', val.REGIO_KOD) && val.REGIO_KOD == regio_) {
                regio.push({
                    REGIO_KOD: val.REGIO_KOD,
                    REGIO: val.REGIO,
                    SORT: asWithoutAccent(val.REGIO)
                });
            }
        });
    }
    if (according == 'kisterseg') {
        $.each(jsn, function (key, val) {
            if (val.KISTERSEG_KOD == fill) {
                helyseg.push({
                    HELYSEG_KOD: val.HELYSEG_KOD,
                    HELYSEG: val.HELYSEG,
                    SORT: asWithoutAccent(val.HELYSEG)
                });
            }
            if (!in_array_r(kisterseg, 'KISTERSEG_KOD', val.KISTERSEG_KOD) && val.KISTERSEG_KOD == $('#kisterseg').val()) {
                megye_ = val.MEGYE_KOD;
                regio_ = val.REGIO_KOD;
                kisterseg.push({
                    KISTERSEG_KOD: val.KISTERSEG_KOD,
                    KISTERSEG: val.KISTERSEG,
                    SORT: asWithoutAccent(val.KISTERSEG)
                });
            }
            if (!in_array_r(megye, 'MEGYE_KOD', val.MEGYE_KOD) && val.MEGYE_KOD == megye_) {
                megye.push({
                    MEGYE_KOD: val.MEGYE_KOD,
                    MEGYE: val.MEGYE,
                    SORT: asWithoutAccent(val.MEGYE)
                });
            }
            if (!in_array_r(regio, 'REGIO_KOD', val.REGIO_KOD) && val.REGIO_KOD == regio_) {
                regio.push({
                    REGIO_KOD: val.REGIO_KOD,
                    REGIO: val.REGIO,
                    SORT: asWithoutAccent(val.REGIO)
                });
            }
        });
    }
    if (according == 'helyseg') {
        $.each(jsn, function (key, val) {
            if (val.HELYSEG_KOD == fill) {
                kisterseg_ = val.KISTERSEG_KOD;
                megye_ = val.MEGYE_KOD;
                regio_ = val.REGIO_KOD;
                helyseg.push({
                    HELYSEG_KOD: val.HELYSEG_KOD,
                    HELYSEG: val.HELYSEG
                });
            }
            if (!in_array_r(kisterseg, 'KISTERSEG_KOD', val.KISTERSEG_KOD) && val.KISTERSEG_KOD == kisterseg_) {
                kisterseg.push({
                    KISTERSEG_KOD: val.KISTERSEG_KOD,
                    KISTERSEG: val.KISTERSEG
                });
            }
            if (!in_array_r(megye, 'MEGYE_KOD', val.MEGYE_KOD) && val.MEGYE_KOD == megye_) {
                megye.push({
                    MEGYE_KOD: val.MEGYE_KOD,
                    MEGYE: val.MEGYE
                });
            }
            if (!in_array_r(regio, 'REGIO_KOD', val.REGIO_KOD) && val.REGIO_KOD == regio_) {
                regio.push({
                    REGIO_KOD: val.REGIO_KOD,
                    REGIO: val.REGIO
                });
            }
        });
    }
//alert(regio);
    out = '<option value="0">==== ' + nyelv['minden'] + ' ====</option>';
    var regio_out = '';
    var megye_out = '';
    var kisterseg_out = '';
    var helyseg_out = '';
    regio.sort((function (index) {
        return function (a, b) {
            return (a[index] === b[index] ? 0 : (a[index] < b[index] ? -1 : 1));
        };
    })('SORT'));
    megye.sort((function (index) {
        return function (a, b) {
            return (a[index] === b[index] ? 0 : (a[index] < b[index] ? -1 : 1));
        };
    })('SORT'));
    kisterseg.sort((function (index) {
        return function (a, b) {
            return (a[index] === b[index] ? 0 : (a[index] < b[index] ? -1 : 1));
        };
    })('SORT'));
    helyseg.sort((function (index) {
        return function (a, b) {
            return (a[index] === b[index] ? 0 : (a[index] < b[index] ? -1 : 1));
        };
    })('SORT'));
    $.each(regio, function (key, val) {
        regio_out += '<option value="' + val.REGIO_KOD + '">' + val.REGIO + '</option>';
    });
    $.each(megye, function (key, val) {
        megye_out += '<option value="' + val.MEGYE_KOD + '">' + val.MEGYE + '</option>';
    });
    $.each(kisterseg, function (key, val) {
        kisterseg_out += '<option value="' + val.KISTERSEG_KOD + '">' + val.KISTERSEG + '</option>';
    });
    $.each(helyseg, function (key, val) {
        helyseg_out += '<option value="' + val.HELYSEG_KOD + '">' + val.HELYSEG + '</option>';
    });
    $('#regio').html(out + regio_out);
    $('#megye').html(out + megye_out);
    $('#kisterseg').html(out + kisterseg_out);
    $('#helyseg').html(out + helyseg_out);

    if (according == 'regio') {
        $('#regio').val(fill);
    }
    if (according == 'megye') {
        $('#regio').val(regio_);
        $('#megye').val(fill);
    }
    if (according == 'kisterseg') {
        $('#regio').val(regio_);
        $('#megye').val(megye_);
        $('#kisterseg').val(fill);
    }
    if (according == 'helyseg') {
        $('#regio').val(regio_);
        $('#megye').val(megye_);
        $('#kisterseg').val(kisterseg_);
        $('#helyseg').val(fill);
    }
    getCount();
    wait(0);
    return true;
}

function asWithoutAccent(s) {
    var from = "¡…⁄’€”‹÷Õ·È˙ı˚Û¸ˆÌ", to = "AEUOUOUOIaeuououoi";
    var ret = '';
    for (var i = 0, len = s.length; i < len; i++) {
        var t = from.indexOf(s.charAt(i));
        ret += t == -1 ? s.charAt(i) : to.charAt(t);
    }
    return ret.toUpperCase();
}