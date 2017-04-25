// unblock when ajax activity stops
$(function () {
	$('#btn_search').click(function() {
		loadGrid();
		refreshGrid();
		getCount();
	});
});

function loadGrid()
{
	var params = getParams();
	
	var col_names_old = [
		'Alintézkedés / Település<br>Pályázó neve / Projekt megnevezése:',
		'Támogatási döntés<br>dátuma:',
		'Megítélt támogatás<br>(HUF):'
	];
	
	var col_names = [
		nyelv['alintezkedes_telepules'] + ':<br>' + nyelv['palyazo_neve_projekt_megnevezese'] + ':',
		nyelv['tamogatasi_dontes_datuma_br'] + ':',
		nyelv['megitelt_tamogatas_huf_br'] + ':'
	];
	
	$('#grid').jqGrid({
		url: '?node=list',
		datatype: 'json',
		mtype: 'POST',
		postData: params,
		colNames: col_names,
		colModel: [
			{name: 'NEV', sortable: true, width: 315},
			{name: 'DONTES_DATUM', sortable: true, width: 120, align: 'right'},
			{name: 'DONTES_OSSZEG', sortable: true, width: 120, align: 'right'}
		],
		pager: '#pager',
		shrinkToFit: true,
		rowNum: 10,
		rowList: [10, 20, 50, 100],
		sortname: 'NEV',
		sortorder: 'asc',
		viewrecords: true,
		gridview: true,
		autoencode: false,
		caption: '',
		height: '100%',
		beforeRequest: function() {
			$('#btn_search').attr('disabled', 'disabled');
		    $.blockUI({ message: '<img src="assets/img/ajax-loader.gif" />' });
		} ,
		loadComplete: function () {
			$('#btn_search').removeAttr('disabled');
		}
	});

	return true;
}

function refreshGrid()
{
	var params = getParams();
	hide();

	$('#grid').setGridParam({postData: params, page:1}).trigger('reloadGrid');

	return true;
}

function getParams() {

	var p = {
		  'ttipus':        $('#ttipus').val()
		, 'tkod':          $('#tkod').val()
		, 'forras':        $('#forras').val()
		, 'forras_uj': 	   $('#forras_uj').val()
		, 'op_type': 	   $('input[name=op_type]:checked').val()
		, 'op_nev': 	   $('#op_nev').val()
		, 'kitoresi_pont': $('#kitoresi_pont').val()
		, 'eupik_nev':     $('#eupik_nev').val()
		, 'kiiras_eve':    $('#kiiras_eve').val()
		, 'palyazo_nev':   $('#palyazo_nev').val()
		, 'print': 		   $('#print').val()
		, 'id_szerv': 	   $('#id_szerv').val()
	};

	return p;
}
