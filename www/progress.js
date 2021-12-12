var progwidth  = 950;
var progheight = 14;
var progtop    = 2;
var run_active = 1;
var kfiles     = 0;
var last_kfile = 1;
var processed_lines = Array(100).fill(0);  /* capacity of 20,000 variants */
var summary_etag;

function catwidth (variable, queued) {
	if (variable == 0)
		return 0;
	var width = variable * progwidth / queued;
	return (width < 1) ? 1 : Math.round (width);
}

function maxcatwidth(A, B, C, D, queued) {
	var cat = new Array();
	cat[0] = catwidth (A, queued);
	cat[1] = catwidth (B, queued);
	cat[2] = catwidth (C, queued);
	cat[3] = catwidth (D, queued);
	cat.sort(function(a,b){return a-b});
	return (progwidth - cat[0] - cat[1] - cat[2]);
}

function minidraw(x, context, color, queued, variable, mcw) {
	var width = catwidth (variable, queued);
	if (width == 0)
		return (0);
	if (width > mcw)
		width = mcw;
	context.fillStyle = color;
	context.fillRect(x, progtop + 1, width, progheight + 2);
	return (width);
}

function update_canvas(stats) {
	var queued = stats.queued;
	var built = stats.built;
	var failed = stats.failed;
	var skipped = stats.skipped;
	var ignored = stats.ignored;

	var canvas = document.getElementById('progressbar');
	if (canvas.getContext === undefined) {
		/* Not supported */
		return;
	}

	var context = canvas.getContext('2d');

	context.fillStyle = '#D8D8D8';
	context.fillRect(0, progtop + 1, progwidth, progheight + 2);
	var x = 0;
	var mcw = maxcatwidth (built, failed, ignored, skipped, queued);
	x += minidraw(x, context, "#339966", queued, built, mcw);
	x += minidraw(x, context, "#CC0033", queued, failed, mcw);
	x += minidraw(x, context, "#FFCC33", queued, ignored, mcw);
	x += minidraw(x, context, "#CC6633", queued, skipped, mcw);
}

function filter (txt) {
	$('#report input').val (txt).trigger('search');
}

function process_summary(data) {
	var html;
	var RB = '<tr>';
	var RE = '</tr>';
	var B = '<td>';
	var E = '</td>';

	kfiles = parseInt (data.kfiles);
	run_active = parseInt (data.active);
	$('#profile').html(data.profile);
	$('#kickoff').html(data.kickoff);
	$('#polling').html(run_active ? "Active" : "Complete");
	if (data.stats) {
		$.each(data.stats, function(status, count) {
			html = count;
			$('#stats_' + status).html(html);
		});
		update_canvas (data.stats);
	}

	$('#builders_body tbody').empty();
	for (n = 0; n < data.builders.length; n++) {
		var trow = RB + '<td class="b' + data.builders[n].ID + 
			'" onclick="filter(\'[' + data.builders[n].ID +
			']\')" title="Click to filter for work done by builder ' +
			data.builders[n].ID + '">'
			  + data.builders[n].ID + E +
			B + data.builders[n].elapsed + E +
			B + data.builders[n].phase + E +
			B + data.builders[n].origin + E +
			B + data.builders[n].lines + E +
			RE;
		$('#builders_body tbody').append (trow);
	}
}

function digit2(n){
	return n > 9 ? "" + n: "0" + n;
}

function logfile (origin) {
	var parts = origin.split(':');
	return 'logs/' + parts[0] + '___' + parts[1] + '.log';
}

function format_result (result) {
	return '<div class="' + result + ' result">' + result + '<div>';
}

function format_entry (entry, origin) {
	return '<span class="entry" onclick="filter(\'' + origin+ '\')">' +
		entry + '</span>';
}

function information (result, origin, info) {
	var parts;
	if (result == "built") {
		return '<a href="' + logfile (origin) + '">logfile</a>';
	} else if (result == "failed") {
		parts = info.split(':');
		return 'Failed ' + parts[0] + ' phase (<a href="' + logfile (origin) +
			'">logfile</a>)';
	} else if (result == "skipped") {
		return 'Issue with ' + info;
	} else if (result == "ignored") {
		parts = info.split(':|:');
		return parts[0];
	} else {
		return "??";
	}
}

function skip_info (result, info) {
	var parts;
	if (result == "failed") {
		parts = info.split(':');
		return parts[1];
	} else if (result == "ignored") {
		parts = info.split(':|:');
		return parts[1];
	} else {
		return "";
	}
}

function originlink (bucket, origin) {
	var parts = origin.split(':');
	var ravenlink = '<a title="Description of the ' + parts[1] +
	' variant of the ' + parts[0] + ' port." href="http://www.ravenports.com/catalog/bucket_' +
	bucket + '/' + parts[0] + '/' + parts[1] + '">' + origin + '</a>';
	return ravenlink;
}

function process_history_file(data, k) {
	var T = $('#report_table').DataTable();
	for (n = processed_lines[k-1]; n < data.length; n++) {
		var trow = [];
		trow.push(format_entry (data[n].entry, data[n].origin));
		trow.push(data[n].elapsed);
		trow.push('[' + data[n].ID + ']');
		trow.push(format_result (data[n].result));
		trow.push(originlink (data[n].bucket, data[n].origin));
		trow.push(information (data[n].result, data[n].origin, data[n].info));
		trow.push(skip_info (data[n].result, data[n].info));
		trow.push(data[n].duration);
		T.row.add(trow);
	}
	processed_lines[k-1] = data.length;
}

function cycle () {
	if (run_active) {
		setTimeout(update_summary_and_builders, 2000);
	} else {
		$('#builders_zone_2').fadeOut(2500);
		$('#main').css('border-top', '1px solid #404066');
	}
}

function update_history_success(kfile) {
	if (kfile == kfiles) {
		$('#report_table').DataTable().draw(false);
		cycle();
	} else {
		last_kfile = kfile + 1;
		update_history();
	}
}

function update_history() {
	if (kfiles == 0) {
		cycle();
		return;
	}
	$.ajax({
		url: digit2(last_kfile) + '_history.json',
		dataType: 'json',
		cache: false
	}).done(function(data) {
		process_history_file(data, last_kfile);
		update_history_success (last_kfile);
	}).fail(function(data) {
		/* May not be there yet, try again shortly */
		setTimeout(update_history, 750);
	})
}

function update_summary_and_builders() {
	var fresh_data = 0;
	$.ajax({
		url: 'summary.json',
		dataType: 'json',
		cache: false
	}).done(function(data, status, jqXHR) {
		if (typeof(data) === 'object') {
			var lastmod = jqXHR.getResponseHeader("ETag");
			if (lastmod != summary_etag) {
				fresh_data = 1;
				summary_etag = lastmod;
				process_summary(data);
				update_history();
			}
		}
		if (!fresh_data) {
			setTimeout(update_summary_and_builders, 750);
		}
	}).fail(function(data) {
		/* Missing file, try again shortly */
		setTimeout(update_summary_and_builders, 750);
	});
}

$(document).ready(function() {
	initialize_database();
	update_summary_and_builders();
})

$(document).bind("keydown", function(e) {
  /* Disable F5 refreshing since this is AJAX driven. */
  if (e.which == 116) {
    e.preventDefault();
  }
});

/*
 * Basic initialization of datatable, run once.
 */
function initialize_database() {
	$('#report_table').DataTable(
	{
		pageLength: 20,
		lengthMenu: [10, 20, 50, 100, 200],
		columns: [
			{ orderable: true,  searchable: false, order: 'asc', type: 'html-num' },
			{ orderable: false, searchable: false },
			{ orderable: false, searchable: true },
			{ orderable: false, searchable: true },
			{ orderable: true,  searchable: true,  order: 'asc' },
			{ orderable: false, searchable: true },
			{ orderable: true,  searchable: false, order: 'desc', type: 'html-num' },
			{ orderable: true,  searchable: false, order: 'desc', type: 'duration' },
		],
		order: [[0, 'desc']]
	}
	);
}

/*
 * Custom sort for duration (ascending)
 */
 jQuery.fn.dataTableExt.oSort['duration-asc'] = function(a,b) {
	 var aparts = a.split(':');
	 var bparts = b.split(':');
	 var asecs = 0;
	 var bsecs = 0;
	 if (aparts[0] != '--') {
		 asecs = parseInt(aparts[2], 10) + parseInt(aparts[1], 10) * 60 + parseInt(aparts[0], 10) * 3600;
	 }
	 if (bparts[0] != '--') {
		 bsecs = parseInt(bparts[2], 10) + parseInt(bparts[1], 10) * 60 + parseInt(bparts[0], 10) * 3600;
	 }
	 if (asecs == bsecs) { return 0; }
	 return (asecs < bsecs) ? -1 : 1;
}

/*
 * Custom sort for duration (descending)
 */
 jQuery.fn.dataTableExt.oSort['duration-desc'] = function(a,b) {
	 var aparts = a.split(':');
	 var bparts = b.split(':');
	 var asecs = 0;
	 var bsecs = 0;
	 if (aparts[0] != '--') {
		 asecs = parseInt(aparts[2], 10) + parseInt(aparts[1], 10) * 60 + parseInt(aparts[0], 10) * 3600;
	 }
	 if (bparts[0] != '--') {
		 bsecs = parseInt(bparts[2], 10) + parseInt(bparts[1], 10) * 60 + parseInt(bparts[0], 10) * 3600;
	 }
	 if (asecs == bsecs) { return 0; }
	 return (asecs > bsecs) ? -1 : 1;
}
