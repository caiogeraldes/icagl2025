require("tidyverse")
require("rethinking")
require("ggdag")


# Fixing errors in rethinking
setMethod("pairs", "ulam", function(x, n = 200, alpha = 0.7, cex = 0.7, pch = 16, adj = 1, pars, ...) {
  # require(rstan)
  if (missing(pars))
    posterior <- extract.samples(x)
  else
    posterior <- extract.samples(x, pars = pars)
  if (!missing(pars)) {
    # select out named parameters
    p <- list()
    for (k in pars) p[[k]] <- posterior[[k]]
    posterior <- p
  }
  panel.dens <- function(x, ...) {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5))
    h <- density(x, adj = adj)
    y <- h$y
    y <- y / max(y)
    abline(v = 0, col = "gray", lwd = 0.5)
    lines(h$x, y)
  }
  panel.2d <- function(x, y, ...) {
    i <- sample(1:length(x), size = n)
    abline(v = 0, col = "gray", lwd = 0.5)
    abline(h = 0, col = "gray", lwd = 0.5)
    dcols <- densCols(x[i], y[i])
    dcols <- sapply(dcols, function(k) col.alpha(k, alpha))
    points(x[i], y[i], col = dcols, ...)
  }
  panel.cor <- function(x, y, ...) {
    k <- cor(x, y)
    cx <- sum(range(x)) / 2
    cy <- sum(range(y)) / 2
    text(cx, cy, round(k, 2), cex = 2 * exp(abs(k)) / exp(1))
  }
  pairs(posterior, cex = cex, pch = pch, upper.panel = panel.2d, lower.panel = panel.cor, diag.panel = panel.dens, ...)
})

# Adding POS
pred_a_pos <- c(
  "P", "P", "P", "N", "P", "P", "N", "P", "P", "P", "N", "A", "P", "P", "P",
  "N", "N", "A", "P", "P", "P", "P", "P", "P", "P", "P", "P", "P", "A", "A",
  "P", "P", "P", "P", "P", "P", "P", "P", "P", "N", "P", "P", "P", "P", "P",
  "P", "P", "A", "P", "P", "P", "P", "P", "A", "P", "P", "P", "A", "P", "P",
  "P", "P", "P", "P", "P", "P", "P", "N", "A", "P", "P", "A", "P", "P", "P",
  "P", "P", "P", "P", "P", "P", "P", "P", "P", "P", "P", "P", "P", "P", "P",
  "P", "P", "P", "P", "P", "P", "A", "P", "P", "P", "P", "P", "P", "N", "A",
  "P", "A", "A", "P", "P", "P", "P", "P", "P", "P", "P", "P", "P", "P", "P",
  "P", "P", "P", "A", "P", "P", "P", "P", "P", "N", "P", "P", "P", "A", "P",
  "N", "P", "P", "P", "P", "P", "P", "P", "P", "N", "P", "P", "A", "P", "P",
  "P", "P", "P", "P", "P", "P", "P", "P", "P", "P", "P", "P", "P", "P", "P",
  "P", "P", "P", "P", "A", "P", "P", "P", "P", "P", "P", "P", "P", "A", "P",
  "P", "P", "P", "P", "P", "P", "P", "P", "P", "P", "P", "P", "P", "P", "P",
  "P", "A", "P", "P", "N", "N", "A", "A", "P", "P", "P", "P", "P", "P", "P",
  "N", "A", "P", "P", "P", "P", "A", "A", "P", "A", "P", "P", "A", "A", "A",
  "A", "P", "P", "A", "A", "N", "A", "N", "P", "A", "A", "N", "N", "A", "A",
  "A", "P", "A", "P", "P", "A", "P", "P", "P", "P", "P", "P", "P", "P", "P",
  "P", "P", "P", "P", "P", "P", "P", "P", "P", "P", "P", "A", "P", "A", "P",
  "P", "P", "P", "P", "P", "P", "P", "P", "P", "P", "P", "P", "P", "P", "P",
  "P", "P", "P", "P", "A", "P", "P", "P", "N", "P", "P", "A", "P", "P", "P",
  "P", "P", "P", "P", "A", "P", "P", "A", "P", "P", "P", "P", "P", "P", "A",
  "P", "P", "P", "P", "P", "A", "P", "P", "P", "N", "A", "P", "P", "P", "P",
  "P", "P", "P", "P", "P", "P", "P", "P", "P", "P", "P", "P", "P"
)

pred_a <- c(
  "παρελθόντι", "τιμωρησαμένῳ", "ἀκούοντι", "ἀδελφῷ", "νέῳ μὲν ὄντι",
  "ἐγκαταληφθέντι", "πορθμεῖ", "ὑπολογιζομένους", "προκατεγνωκότας",
  "ἀκούοντας", "γαμόρῳ", "σωφρονεστέραν", "ἀκούσαντας", "δοκιμάσαντας",
  "ἀπελθόντι", "ἀνδράσιν... ἀγαθοῖς καὶ δικαίοις", "ἀνδράσιν ἀγαθοῖς",
  "δημοτικῷ", "εἰσιόντι", "διαλλαχθέντι", "ἴσους καὶ κοινοὺς... ἐπιστάτας",
  "ἀκροασαμένους", "προσέχοντας", "ἀναμνησθέντας", "ἀντιδράσαντα",
  "διακονοῦσαν", "ἀποφήνασι", "φεισαμένοις", "ἀρχαιοτάτοις", "ἀρχαιοτάτοις",
  "ἀρξαμένοις", "ἀρξαμένοις", "καταστήσαντι", "ἐλεήσαντας", "ἀποδεξαμένους",
  "πεισθέντας", "ἀκεσαμένους", "ζητοῦντας", "ἀκούοντας",
  "ταῖσι γυναιξὶν συλλεχθείσαις", "λεγούσαις", "ἐπιτιμήσαντα", "μισθώσαντι",
  "ὁρῶσιν", "γραψαμένοις", "κληρονόμοις", "ποιοῦντι", "εὐδαίμοσιν",
  "φρονοῦντας", "ὄντι δ’ ἀτίμῳ", "βοηθοῦσι", "πάσχοντι", "μαινομένοις",
  "ἀντιπάλους", "φενακισθεῖσιν", "ἔχοντι", "ἀναστρέφουσιν", "ἀχρηστοτέροις",
  "ὑπερημέρῳ γενομένῳ", "οἰομένοις", "ἁψαμένῳ", "ἀκούουσιν", "ἐψηφισμένῃ",
  "θύοντας", "τιμωρησαμένους", "μεγίστοις οὖσιν", "παιδὶ μὲν ὄντι", "συκοφάντῃ",
  "καλοῖς", "ποιησαμένῃ", "ποιησαμένῃ", "κυρίοις", "ποιήσασι", "φέροντα",
  "εἰσιόντας", "ἐγκαταλιπόντας", "λύσασι", "ἀνασχομένους", "μνησθέντας",
  "λογισαμένους", "ἀκούσαντας", "ἀναμνησθέντας", "ὑμᾶς... ποιησαμένους",
  "ἐνθυμουμένους", "ἔχοντι", "τυχόντας", "ἀμυνομένους", "πέμψαντας", "πέμψαντα",
  "διαβάντας", "ψιλώσαντας", "μεταδιώξαντας", "λαβόντα", "διαστάντας",
  "ποιήσαντι", "παρελθόντας", "πολεμίους", "μένοντας", "ἐξαργυρώσαντα",
  "διαταχθέντας", "πολιορκέοντας", "ἐξαναστήσαντας", "ὑπομείναντας",
  "τῷ δικαιοτάτῳ", "Δελφὸν", "ἀδικέοντι", "τιμωρητήρων", "βοηθοὺς",
  "ἡμερολογέοντας", "παραμένοντα", "διδόντα", "συλλαβόντας", "ζῶντα",
  "ἐξανδραποδισαμένους", "λαβόντας", "ἑπομένους", "φρουρήσαντας", "ἑλόντα",
  "πειρωμένοισι", "κελεύοντας", "ζευγνύντας", "ἀναλαβόντας", "ἀναλαβόντας",
  "δυνατὸς", "ἐκτείναντα", "ὑπερβαλλόμενον", "ἐλθόντας", "λαβοῦσι", "μένοντας",
  "ἄνδρας", "κομισαμένοις", "ἐγκαταλιπόντα", "ἐκδόντι", "ἐξάρνῳ",
  "ὄντι προτέρῳ", "γυναικί", "ἐκγόνοις οὖσι", "παρακαλέσαντι",
  "προσαγορευόμενον", "ἐνθυμηθέντας", "ἁφέντι", "γραψαμένῳ", "ταλαιπωρηθεῖσιν",
  "διαλεχθεῖσι", "συμβούλοις", "προσέχοντι", "φρονήσαντας", "λαμπροῖς",
  "σιγῶντι", "δαπανωμένοις", "διαλυσαμένοις", "καταδεεστέρους ὄντας",
  "μεμνημένους", "ἐνθυμουμένους", "ὄντας", "ἐλθόνθ’", "μεμνημένους", "ἀφέντας",
  "τοιούτοις οὖσιν", "λέγοντι", "ἀμελήσαντι", "ὑποδείξοντας", "διορθώσαντα",
  "ἐνθυμηθέντας", "ἐξαναστήσαντας", "βουλομένοις", "πειραθέντας", "λέγοντι",
  "λαβόντι", "ὑβρισταῖς", "αὐτοῖς πάσχουσιν", "μαθούσαις", "ἀπηλλαγμένον",
  "παρελθόντι", "καταστήσαντι", "μαστιγωθεῖσαν", "χρωμένοις", "ἡσυχίαν ἄγοντι",
  "κοινοὺς", "ἐνθυμουμένους", "ἐνθυμουμένους", "ἡγουμένους", "ἐνθυμουμένους",
  "ἀκροασαμένους", "μεμνημένους", "φίλοις οὖσι", "φιλονικοῦσιν",
  "ἐμὲ νεώτερον ὄντα", "διανείμαντας", "σκοποῦντα", "ἐπαινοῦντά", "ἀκούσαντας",
  "παραμένοντα", "καταφυγόντα", "εἰσιόντι", "σκοποῦντας", "κακῷ", "μεθυσθέντι",
  "κακὴν γενομένην", "τύραννον", "σώματι", "τοιούτῳ", "ἀγάμοις", "χρωμένῳ",
  "ἀναστρέφοντα", "λίχνῳ δὲ ὄντι", "πλεονεκτοῦσιν", "πληγέντι",
  "οὑτωσὶ ἀμπεχομένῳ", "παραμυθουμένῳ", "στρατηγῷ", "συνῳδῷ", "τιμωρουμένῳ",
  "ἐπισταμένῳ", "τελεωθέντι", "ἐρωτήσασαν", "ἑτέρῳ", "ἀδιαλύτῳ", "θαρροῦντι",
  "βελτίονί", "σοφῷ ὄντι καὶ ἔχοντι", "κρίναντι", "εὐδαίμονι", "ἀνελέγκτοις",
  "ὑγιέσιν", "ἀξίῳ", "κρατουμένοις", "ἁσμένους", "ἄδικον", "ἀνάνδρῳ", "ὑέσιν",
  "καλῷ", "ἐπαινέτην", "διαφθείροντας", "εὐδαίμονι", "ἡδίστοισιν", "ῥητορικῷ",
  "ἰατρῷ", "ἀστασιάστοις", "δικαίῳ", "θεοφιλεῖ", "ἀσκοῦντας", "εὐδαιμονέστερον",
  "ἡγουμένῳ", "ὁρμήσαντα", "ἀνομιλήτῳ", "λαβόντι", "κραιπαλῶντα", "ὑφειμένῃ",
  "μένοντι", "παρόντι", "ἐλθοῦσιν", "ἀπολογησομένους", "περιμείνασι",
  "καταβάντας", "ποιησαμένους", "ἡσυχάσασι", "ἔχοντας", "πειθομένους",
  "διαμέλλοντας", "καύσαντας", "ἔχοντας", "προσκαθημένους", "διδοῦσι",
  "τραπομένοις", "ἔχουσι", "φίλους", "καταγαγοῦσι", "ἀγαθοῖς", "πολιορκοῦντας",
  "γενομένῳ", "ὀνομασθέντα", "ξυγκτίσαντας", "ἀναγκαζομένοις", "ἔχοντας",
  "ἡσυχάζοντας", "ἔχουσι", "ἐπεξαγαγόντας", "συσκευασαμένοις", "καταστήσαντας",
  "ἔχοντας", "συλλεγεῖσιν", "ποιησαμένους", "παρακαλέσαντας", "πέμψαντας",
  "ὑποστρέψαντας", "πέμψαντας", "καθημένους", "καταστήσαντας", "ὑποσπόνδους",
  "ὁρῶντι", "προκαταλαβοῦσιν", "σωθεῖσιν", "ἀνδρὶ", "κεχριμένῳ", "λαβόντας",
  "πεζοῖς", "ἰδόντι", "ἀπαλλαγέντι", "πιέσαντας", "γενομένῳ", "φίλοις οὖσι",
  "δεηθέντι", "λαβόντι", "ἀγαθοῖς", "ἐλεγχθέντι", "ἄρχοντι φρονίμῳ", "φίλους",
  "θεωρήσοντι", "διαπονησαμένῳ", "συνθεμένους", "κατεχομένῳ", "διαλέγοντας",
  "φυλάττοντα", "ἐλευθέρῳ", "πληρώσαντας", "πράξοντας", "ἐπαινοῦντι",
  "ἀποκτείνασι", "παρέντι", "φίλους", "παρέντι", "ἐλθόντι", "παραγενόμενον",
  "ἀξίῳ... ἀνδρί", "ἄξιος", "λαβόντι", "λαβόντα", "παραγαγόντας",
  "παιανίσαντας", "διηγκυλωμένους", "ἄγοντα", "ἐστεφανωμένοις", "ἔχοντας",
  "ἐνθεμένους", "ἔχοντας", "ἐμμόνοις", "ἐλθόντα", "Ἕλληνας ὄντας", "ἀπειλοῦντα",
  "πεινῶντας", "ἐκβιβάζοντας", "ἐνθυμουμένους"
)

# Loading data
dados <- read_csv("./data_animacy.csv") %>%
  mutate(pos_pred_a = factor(pred_a_pos, levels = c("A", "N", "P")),
    pred_a = pred_a
  )


data <- list(
  "A" = dados$Attr,
  "Cop" = dados$Vinf_Cop,
  "Aut" = as.factor(dados$AUTHOR),
  "P" = as.double(dados$pos_pred_a),
  alpha = rep(2, 2)
)

f3_cop_pos <- alist(
  A ~ bernoulli(p),
  logit(p) <- -pre_a + b_c * Cop + b_p * sum(delta_j[1:P]) + b_a[Aut],
  transpars > a <<- pre_a * -1,
  pre_a ~ lognormal(0, 1),
  b_a[Aut] ~ normal(b_a_bar, s_a),
  b_a_bar ~ normal(0, 1),
  s_a ~ exponential(1),
  b_c ~ normal(0, 1),
  b_p ~ normal(0, 0.5),
  vector[3]:delta_j <<- append_row(0, delta),
  simplex[2]:delta ~ dirichlet(alpha),
  P ~ dordlogit(phi, cutpoints),
  phi <- u + g_c * Cop,
  g_c ~ dnorm(0, 0.5),
  u ~ dnorm(0, 1),
  cutpoints ~ dnorm(0, 1.5)
)

m3 <- ulam(
  flist = f3_cop_pos,
  data = data,
  chains = 8, cores = 4,
  log_lik = TRUE
)
precis(m3)
dashboard(m3)

png("../fala/figs/model3_precis.png")
plot(
  precis(m3, depth = 2, pars = c("a", "b_p", "delta")),
  labels = c("α", "Participle", "Adjective", "Noun")
)
dev.off()

samples <- extract.samples(m3)

HPDI(samples$b_p)

round(HPDI(samples$b_p + samples$delta[, 1]), 2)
round(mean(samples$b_p + samples$delta[, 1]), 2)
round(sd(samples$b_p + samples$delta[, 1]), 2)

round(HPDI(samples$b_p + samples$delta[, 1] + samples$delta[, 2]), 2)
round(mean(samples$b_p + samples$delta[, 1] + samples$delta[, 2]), 2)
round(sd(samples$b_p + samples$delta[, 1] + samples$delta[, 2]), 2)

cor(samples$delta[, 1], samples$delta[, 2])

png("../fala/figs/model3_banddelta.png")
pairs(
  m3,
  pars = c("b_p", "delta"), labels = c("Participle", "Adjective", "Noun")
)
dev.off()

plot(
  precis(m3, depth = 2,
    pars = c("a", "b_a", "b_c", "b_p", "delta")
  ),
  labels = c(
    "α", levels(factor(dados$AUTHOR)), "Copula",
    "Participle", "Adjective", "Noun"
  )
)


# Independence tests
chisq.test(dados$GENRE, dados$Attr)

chisq.test(dados$GENRE, dados$Attr)

dados_modal_vm <- dados %>% filter(VM_MOD)
dados_not_modal_vm <- dados %>% filter(!VM_MOD)


chisq.test(dados_modal_vm$GENRE, dados_modal_vm$Attr)
chisq.test(dados_not_modal_vm$GENRE, dados_not_modal_vm$Attr)

chisq.test(dados_modal_vm$AUTHOR, dados_modal_vm$Attr)
chisq.test(dados_not_modal_vm$AUTHOR, dados_not_modal_vm$Attr)


dados_juridical <- dados %>% filter(GENRE == "juridical")
dados_drama <- dados %>% filter(GENRE == "drama")
dados_historiography <- dados %>% filter(GENRE == "historiography")
dados_philosophical <- dados %>% filter(GENRE == "philosophical_dialogue")
dados_other <- dados %>% filter(GENRE == "other")

chisq.test(dados_juridical$AUTHOR, dados_juridical$Attr)
chisq.test(dados_drama$AUTHOR, dados_drama$Attr)
chisq.test(dados_historiography$AUTHOR, dados_historiography$Attr)
chisq.test(dados_philosophical$AUTHOR, dados_philosophical$Attr)
chisq.test(dados_other$AUTHOR, dados_other$Attr)

chisq.test(dados$VM_MOD, dados$Vinf_Cop)
chisq.test(dados$AUTHOR, dados$Cop)


a <- table(
  dados$DIALECT,
  dados$AUTHOR,
  dados$VM_MOD,
  dados$Attr
)
# Attic
chisq.test(a[1, , 1, ], correct = TRUE)
chisq.test(a[1, , 2, ], correct = TRUE)
# Jonic (Only Herodotus)
chisq.test(a[2, , 1, ], correct = TRUE)
chisq.test(a[2, , 2, ], correct = TRUE)

dag <- dagitty::dagitty("
  dag{
    A[outcome]
    PoS[exposure]
    PossV[exposure]
    WO[exposure]
    Aut -> Dia
    Aut -> Genre
    Aut -> WO
    Cop -> A
    Cop -> PoS
    Dia -> A
    Genre -> PossV
    PoS -> A
    PossV -> A
    PossV -> PoS
    WO -> A
  }
")
