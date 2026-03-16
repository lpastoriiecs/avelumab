// Numeric inputs: solo disparan al perder foco.
// Parcheamos el subscribe del binding ANTES de que Shiny lo use.
// Shiny.js ya está cargado cuando script.js se ejecuta, por lo que
// Shiny.inputBindings.bindingNames['shiny.numberInput'] ya existe.
(function() {
  var b = Shiny.inputBindings.bindingNames['shiny.numberInput'];
  if (!b) return;
  b.binding.subscribe = function(el, callback) {
    $(el).on('change.numberInputBinding', function() { callback(true); });
  };
  b.binding.unsubscribe = function(el) {
    $(el).off('.numberInputBinding');
  };
}());

Shiny.addCustomMessageHandler('inicializar-tooltips', function(tooltips) {
    Object.entries(tooltips).forEach(([id, config]) => {
      const el = document.getElementById(id);
      if (el) {
        el.setAttribute('title', config.text);
        el.setAttribute('data-bs-toggle', 'tooltip');
        el.setAttribute('data-bs-placement', config.posicion);
      }
    });

    // Re-activar tooltips Bootstrap (en caso de haber sido removidos)
    var tooltipTriggerList = [].slice.call(document.querySelectorAll('[data-bs-toggle=\"tooltip\"]'))
    tooltipTriggerList.map(function (tooltipTriggerEl) {
      return new bootstrap.Tooltip(tooltipTriggerEl)
    });
  });


function ajustarTopBar() {
  const topBar = document.querySelector('.top-bar');
  const appContainer = document.querySelector('.app-container');
  const blocker = document.querySelector('.top-bar-shadow-blocker');
  if (topBar && appContainer && blocker) {
    const altura = topBar.offsetHeight;
    appContainer.style.paddingTop = `${altura}px`;
    blocker.style.height = `${altura}px`;
  }
}

$(document).on('click', '#toggleParamBar', function() {
  console.log("Clic en toggleParamBar");
  $('#colInputs').toggleClass('collapsed');
  
 /* if ($('#colInputs').hasClass('visCollapsed')) {
    $('#colInputs').removeClass('visCollapsed');
    $('#colRes').addClass('visCollapsed');
    $('#toggleParamBarTop').addClass('girado');
  } else {
     $('#colInputs').removeClass('#colRes');
    $('#colInputs').addClass('visCollapsed');
    $('#toggleParamBarTop').removeClass('girado');
  }*/
  
    
  if ($('#colInputs').hasClass('collapsed')) {
    $('#colInputs').addClass('visCollapsed');
    $('#colRes').removeClass('visCollapsed');
    $('#toggleParamBarTop').removeClass('girado');
  } else {
     $('#colInputs').removeClass('visCollapsed');
    $('#colRes').addClass('visCollapsed');
    $('#toggleParamBarTop').addClass('girado');
  }
});
$(document).on('click', '#toggleSidebar', function() {
  console.log("Clic en toggleParamBar");
  //if (window.innerWidth < 768) {
    console.log("Entra aca")
    $('#sidebar').toggleClass('collapsed');
    if ($('#sidebar').hasClass('collapsed')) {
      $('.tabbable').removeClass('visCollapsed');
    } else {
      $('.tabbable').addClass('visCollapsed');
    } 
    
  //} else {
  //  $('#sidebar').toggleClass('collapsed');
  //}
});
$(document).on('click', '#toggleParamBarTop', function() {
  console.log("Clic en toggleParamBar");
  if ($('#colInputs').hasClass('visCollapsed')) {
    $('#colInputs').removeClass('visCollapsed');
    $('#colInputs').removeClass('collapsed');
    $('#colRes').addClass('visCollapsed');
    $('#toggleParamBarTop').addClass('girado');
  } else {
     $('#colRes').removeClass('visCollapsed');
    $('#colInputs').addClass('visCollapsed');
    $('#colInputs').addClass('collapsed');
    $('#toggleParamBarTop').removeClass('girado');
  }
});
$(document).on('click', '#navConfiguracion', function() {
  if (window.innerWidth < 768) {
    $('#sidebar').addClass('collapsed');
    $('.tabbable').removeClass('visCollapsed');
  }
  
});
$(document).on('click', '#navVisualizador', function() {
  if (window.innerWidth < 768) {
    $('#sidebar').addClass('collapsed');
    $('.tabbable').removeClass('visCollapsed');
  }
});

$(document).on('click', '#navIntroduccion', function() {
  if (window.innerWidth < 768) {
    $('#sidebar').addClass('collapsed');
    $('.tabbable').removeClass('visCollapsed');
  }
});
document.addEventListener('DOMContentLoaded', () => {
  const contenido = document.getElementById('login1-disclaimerText');
  const boton = document.getElementById('login1-cmdAcepto');
  const sentinel = contenido.querySelector('.scroll-sentinel');

  if (!contenido || !boton || !sentinel) return;

  // Estado inicial
  boton.disabled = true;
  const habilitarSoloUnaVez = true;
  //$("#bInflacion").parent().css("margin-right", "0px");
  $("#bInflacion").parent().parent().css("width", "auto");
  const io = new IntersectionObserver((entries) => {
    const entry = entries[0];
    if (entry.isIntersecting) {
      boton.disabled = false;
      if (habilitarSoloUnaVez) io.disconnect();
    } else if (!habilitarSoloUnaVez) {
      boton.disabled = true;
    }
  }, {
    root: contenido,  // El contenedor que scrollea
    threshold: 0      // El sentinela debe estar 100% visible
  });

  io.observe(sentinel);
});