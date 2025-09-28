;; ------------------------------------------------------------------
;; SISTEM PAKAR REKOMENDASI WISATA
;; -- Menggunakan Database Fakta dan Mesin Pencocokan Dinamis --
;; ------------------------------------------------------------------

;; ==================================================================
;; BAGIAN 1: DEFINISI TEMPLATE
;; ==================================================================

(deftemplate pengguna
   "Informasi preferensi pengguna setelah menjawab kuesioner."
   (slot tipe-wisata (type SYMBOL))
   (slot budget (type SYMBOL))
   (slot jumlah-orang (type SYMBOL))
   (slot aktivitas (type SYMBOL))
)

;; --- TEMPLATE BARU UNTUK DATABASE WISATA ---
;; Menggunakan multislot agar satu tempat bisa punya banyak atribut.
(deftemplate tempat-wisata
    "Struktur untuk data master tempat wisata."
    (slot nama (type STRING))
    (slot lokasi (type STRING))
    (slot deskripsi (type STRING))
    (multislot tipe)        ; Bisa diisi >1, cth: (tipe Alam Petualangan)
    (multislot budget)      ; Bisa diisi >1, cth: (budget Rendah Sedang)
    (multislot cocok-untuk) ; Bisa diisi >1, cth: (cocok-untuk Pasangan Keluarga)
    (multislot aktivitas)   ; Bisa diisi >1, cth: (aktivitas Mendaki Fotografi)
)

(deftemplate rekomendasi
   "Struktur untuk menampilkan hasil rekomendasi."
   (slot nama-tempat (type STRING))
   (slot deskripsi (type STRING))
   (slot alasan (type STRING))
   (slot skor (type INTEGER))  ; Tambahan slot untuk skor agar bisa sorting nanti jika diperlukan
)

;; Template untuk alur interaktif (tidak berubah)
(deftemplate fase-kontrol (slot nama))
(deftemplate jawaban-pengguna (slot kategori) (slot nilai))

;; ==================================================================
;; BAGIAN 2: DATABASE FAKTA TEMPAT WISATA
;; Di sinilah kita mendefinisikan semua data tempat wisata.
;; Sangat mudah untuk menambah data baru di sini.
;; ==================================================================

(deffacts database-wisata
    (tempat-wisata 
        (nama "Gunung Bromo")
        (lokasi "Jawa Timur")
        (deskripsi "Pemandangan matahari terbit magis dan kawah aktif yang menakjubkan.")
        (tipe Alam Petualangan)
        (budget Rendah Sedang)
        (cocok-untuk Sendiri Pasangan Keluarga)
        (aktivitas Mendaki Fotografi)
    )
    (tempat-wisata 
        (nama "Ubud")
        (lokasi "Bali")
        (deskripsi "Pusat seni, budaya, dan ketenangan dengan sawah terasering yang ikonik.")
        (tipe Budaya Alam)
        (budget Sedang Tinggi)
        (cocok-untuk Pasangan Keluarga)
        (aktivitas Kuliner Sejarah Fotografi)
    )
    (tempat-wisata 
        (nama "Candi Borobudur")
        (lokasi "Jawa Tengah")
        (deskripsi "Candi Buddha terbesar di dunia, sebuah mahakarya arsitektur kuno.")
        (tipe Budaya)
        (budget Rendah Sedang)
        (cocok-untuk Sendiri Pasangan Keluarga)
        (aktivitas Sejarah Fotografi)
    )
    (tempat-wisata 
        (nama "Labuan Bajo")
        (lokasi "Nusa Tenggara Timur")
        (deskripsi "Gerbang menuju Pulau Komodo dan surga diving dengan pemandangan pulau-pulau eksotis.")
        (tipe Petualangan Pantai Alam)
        (budget Tinggi)
        (cocok-untuk Pasangan Petualang)
        (aktivitas Berenang Mendaki Fotografi)
    )
    (tempat-wisata 
        (nama "Raja Ampat")
        (lokasi "Papua Barat")
        (deskripsi "Kepulauan dengan keindahan bawah laut paling kaya di dunia dan gugusan pulau karst yang memukau.")
        (tipe Pantai Alam)
        (budget Tinggi)
        (cocok-untuk Pasangan Petualang)
        (aktivitas Berenang Fotografi)
    )
    (tempat-wisata 
        (nama "Danau Toba")
        (lokasi "Sumatera Utara")
        (deskripsi "Danau vulkanik terbesar di dunia dengan Pulau Samosir di tengahnya yang kaya akan budaya Batak.")
        (tipe Alam Budaya)
        (budget Rendah Sedang)
        (cocok-untuk Keluarga Pasangan)
        (aktivitas Berenang Kuliner Sejarah)
    )
    (tempat-wisata 
        (nama "Kota Yogyakarta")
        (lokasi "D.I. Yogyakarta")
        (deskripsi "Pusat kebudayaan Jawa yang kaya akan sejarah, seni, dan kuliner khas.")
        (tipe Budaya)
        (budget Rendah Sedang)
        (cocok-untuk Sendiri Pasangan Keluarga)
        (aktivitas Kuliner Sejarah)
    )
)

;; Fakta awal untuk memulai program (tidak berubah)
(deffacts mulai-program
    (fase-kontrol (nama mulai))
)

;; ==================================================================
;; BAGIAN 3: ATURAN-ATURAN INTERAKTIF (DENGAN VALIDASI INPUT)
;; Ditambahkan validasi untuk memastikan input sesuai opsi yang tersedia.
;; Jika input salah, tanya lagi.
;; ==================================================================

(defrule mulai-interaksi 
   ?f <- (fase-kontrol (nama mulai)) 
   => 
   (retract ?f) 
   (printout t crlf "=============================================" crlf " Selamat Datang di Sistem Pakar Rekomendasi Wisata!" crlf " Jawab beberapa pertanyaan berikut untuk mendapatkan rekomendasi." crlf "=============================================" crlf crlf) 
   (assert (fase-kontrol (nama tanya-tipe-wisata)))
)

(defrule tanya-tipe-wisata 
   ?f <- (fase-kontrol (nama tanya-tipe-wisata)) 
   => 
   (retract ?f) 
   (printout t "1. Tipe wisata apa yang Anda sukai? (Pilih: Alam, Budaya, Pantai, Petualangan)" crlf "> ") 
   (assert (jawaban-pengguna (kategori tipe-wisata) (nilai (sym-cat (readline)))))
)

(defrule validasi-tipe-wisata-salah
   ?f <- (fase-kontrol (nama tanya-tipe-wisata)) ; Tidak ada, karena sudah retract
   ?j <- (jawaban-pengguna (kategori tipe-wisata) (nilai ?nilai))
   (test (not (member$ ?nilai (create$ Alam Budaya Pantai Petualangan))))
   =>
   (retract ?j)
   (printout t "Input tidak valid. Silakan pilih dari opsi yang tersedia." crlf)
   (assert (fase-kontrol (nama tanya-tipe-wisata)))
)

(defrule validasi-tipe-wisata-benar
   ?j <- (jawaban-pengguna (kategori tipe-wisata) (nilai ?nilai))
   (test (member$ ?nilai (create$ Alam Budaya Pantai Petualangan)))
   =>
   (assert (fase-kontrol (nama tanya-budget)))
)

(defrule tanya-budget 
   ?f <- (fase-kontrol (nama tanya-budget)) 
   => 
   (retract ?f) 
   (printout t crlf "2. Bagaimana budget Anda? (Pilih: Rendah, Sedang, Tinggi)" crlf "> ") 
   (assert (jawaban-pengguna (kategori budget) (nilai (sym-cat (readline)))))
)

(defrule validasi-budget-salah
   ?j <- (jawaban-pengguna (kategori budget) (nilai ?nilai))
   (test (not (member$ ?nilai (create$ Rendah Sedang Tinggi))))
   =>
   (retract ?j)
   (printout t "Input tidak valid. Silakan pilih dari opsi yang tersedia." crlf)
   (assert (fase-kontrol (nama tanya-budget)))
)

(defrule validasi-budget-benar
   ?j <- (jawaban-pengguna (kategori budget) (nilai ?nilai))
   (test (member$ ?nilai (create$ Rendah Sedang Tinggi)))
   =>
   (assert (fase-kontrol (nama tanya-jumlah-orang)))
)

(defrule tanya-jumlah-orang 
   ?f <- (fase-kontrol (nama tanya-jumlah-orang)) 
   => 
   (retract ?f) 
   (printout t crlf "3. Dengan siapa Anda berlibur? (Pilih: Sendiri, Pasangan, Keluarga, Petualang)" crlf "> ") 
   (assert (jawaban-pengguna (kategori jumlah-orang) (nilai (sym-cat (readline)))))
)

(defrule validasi-jumlah-orang-salah
   ?j <- (jawaban-pengguna (kategori jumlah-orang) (nilai ?nilai))
   (test (not (member$ ?nilai (create$ Sendiri Pasangan Keluarga Petualang))))
   =>
   (retract ?j)
   (printout t "Input tidak valid. Silakan pilih dari opsi yang tersedia." crlf)
   (assert (fase-kontrol (nama tanya-jumlah-orang)))
)

(defrule validasi-jumlah-orang-benar
   ?j <- (jawaban-pengguna (kategori jumlah-orang) (nilai ?nilai))
   (test (member$ ?nilai (create$ Sendiri Pasangan Keluarga Petualang)))
   =>
   (assert (fase-kontrol (nama tanya-aktivitas)))
)

(defrule tanya-aktivitas 
   ?f <- (fase-kontrol (nama tanya-aktivitas)) 
   => 
   (retract ?f) 
   (printout t crlf "4. Aktivitas apa yang Anda cari? (Pilih: Mendaki, Fotografi, Berenang, Kuliner, Sejarah)" crlf "> ") 
   (assert (jawaban-pengguna (kategori aktivitas) (nilai (sym-cat (readline)))))
)

(defrule validasi-aktivitas-salah
   ?j <- (jawaban-pengguna (kategori aktivitas) (nilai ?nilai))
   (test (not (member$ ?nilai (create$ Mendaki Fotografi Berenang Kuliner Sejarah))))
   =>
   (retract ?j)
   (printout t "Input tidak valid. Silakan pilih dari opsi yang tersedia." crlf)
   (assert (fase-kontrol (nama tanya-aktivitas)))
)

(defrule validasi-aktivitas-benar
   ?j <- (jawaban-pengguna (kategori aktivitas) (nilai ?nilai))
   (test (member$ ?nilai (create$ Mendaki Fotografi Berenang Kuliner Sejarah)))
   =>
   (assert (fase-kontrol (nama kumpulkan-jawaban)))
)

(defrule kumpulkan-jawaban-dan-proses 
   ?f <- (fase-kontrol (nama kumpulkan-jawaban)) 
   ?j1 <- (jawaban-pengguna (kategori tipe-wisata) (nilai ?tipe)) 
   ?j2 <- (jawaban-pengguna (kategori budget) (nilai ?budget)) 
   ?j3 <- (jawaban-pengguna (kategori jumlah-orang) (nilai ?jumlah)) 
   ?j4 <- (jawaban-pengguna (kategori aktivitas) (nilai ?aktivitas)) 
   => 
   (retract ?f ?j1 ?j2 ?j3 ?j4) 
   (printout t crlf "Terima kasih! Kami sedang memproses jawaban Anda..." crlf) 
   (assert (pengguna (tipe-wisata ?tipe) (budget ?budget) (jumlah-orang ?jumlah) (aktivitas ?aktivitas)))
   (assert (fase-kontrol (nama evaluasi-selesai)))  ; Flag untuk menandai evaluasi siap dicek
)

;; ==================================================================
;; BAGIAN 4: ATURAN MESIN PENCOCOKAN & TAMPILAN HASIL
;; Perbaikan: Hapus retract ?pref dari aturan evaluasi agar semua tempat dievaluasi.
;; Tambah slot skor di rekomendasi.
;; Tambah aturan untuk handle jika tidak ada rekomendasi.
;; Aturan tampilkan diganti untuk print semua sekaligus dengan loop.
;; ==================================================================

(defrule evaluasi-tempat-wisata
    "Aturan ini membandingkan preferensi pengguna dengan setiap tempat wisata di database,
     menghitung skor, dan hanya merekomendasikan yang paling relevan."

    ; Pola yang harus cocok (LHS)
    (pengguna 
        (tipe-wisata ?tipe-user) 
        (budget ?budget-user)
        (jumlah-orang ?jumlah-user)
        (aktivitas ?aktivitas-user))
    
    (tempat-wisata 
        (nama ?nama)
        (lokasi ?lokasi)
        (deskripsi ?deskripsi)
        (tipe $?tipe-db)
        (budget $?budget-db)
        (cocok-untuk $?cocok-db)
        (aktivitas $?aktivitas-db))
    =>
    ; Aksi dan Logika Perhitungan Skor (RHS)
    (bind ?skor 0) ; Mulai skor dari 0
    (bind ?alasan-cocok "") ; Mulai alasan dari kosong

    ; Cek kecocokan TIPE WISATA (skor +2 jika cocok, ini prioritas tinggi)
    (if (member$ ?tipe-user ?tipe-db) then
        (bind ?skor (+ ?skor 2))
        (bind ?alasan-cocok (str-cat ?alasan-cocok "Tipe Wisata, "))
    )
    ; Cek kecocokan AKTIVITAS (skor +2 jika cocok, ini juga prioritas tinggi)
    (if (member$ ?aktivitas-user ?aktivitas-db) then
        (bind ?skor (+ ?skor 2))
        (bind ?alasan-cocok (str-cat ?alasan-cocok "Aktivitas, "))
    )
    ; Cek kecocokan BUDGET (skor +1 jika cocok)
    (if (member$ ?budget-user ?budget-db) then
        (bind ?skor (+ ?skor 1))
        (bind ?alasan-cocok (str-cat ?alasan-cocok "Budget, "))
    )
    ; Cek kecocokan JUMLAH ORANG (skor +1 jika cocok)
    (if (member$ ?jumlah-user ?cocok-db) then
        (bind ?skor (+ ?skor 1))
        (bind ?alasan-cocok (str-cat ?alasan-cocok "Tipe Perjalanan, "))
    )
    
    ; --- AMBANG BATAS (THRESHOLD) ---
    ; Hanya buat rekomendasi jika skornya 3 atau lebih.
    (if (>= ?skor 3) then
        (bind ?alasan-final (sub-string 1 (- (str-length ?alasan-cocok) 2) ?alasan-cocok)) ; Menghapus koma terakhir
        (assert (rekomendasi 
                    (nama-tempat (str-cat ?nama ", " ?lokasi))
                    (deskripsi ?deskripsi)
                    (alasan (str-cat "Rekomendasi ini cocok berdasarkan preferensi: " ?alasan-final "."))
                    (skor ?skor)
                ))
    )
)

;; Aturan untuk menampilkan semua hasil sekaligus (menggunakan loop)
(defrule tampilkan-hasil
   (declare (salience -10))  ; Salience rendah agar fired setelah semua evaluasi
   ?flag <- (fase-kontrol (nama evaluasi-selesai))
   (exists (rekomendasi))  ; Hanya fired jika ada setidaknya satu rekomendasi
   ?pref <- (pengguna)
   =>
   (retract ?flag ?pref)
   (printout t crlf "=============================================" crlf)
   (printout t "  SISTEM MENEMUKAN REKOMENDASI UNTUK ANDA!" crlf)
   (printout t "=============================================" crlf crlf)
   
   ; Loop untuk print semua rekomendasi (CLIPS tidak punya sort built-in, jadi print as-is)
   (do-for-all-facts ((?r rekomendasi)) TRUE
      (printout t "Nama Tempat : " ?r:nama-tempat crlf)
      (printout t "Deskripsi   : " ?r:deskripsi crlf)
      (printout t "Alasan      : " ?r:alasan crlf)
      (printout t "Skor Cocok  : " ?r:skor crlf crlf)
      (retract ?r)
   )
   (printout t "=============================================" crlf)
)

;; Aturan untuk handle jika tidak ada rekomendasi
(defrule tidak-ada-rekomendasi
   (declare (salience -10))
   ?flag <- (fase-kontrol (nama evaluasi-selesai))
   (not (rekomendasi))
   ?pref <- (pengguna)
   =>
   (retract ?flag ?pref)
   (printout t crlf "=============================================" crlf)
   (printout t "  Maaf, tidak ada rekomendasi yang cocok dengan preferensi Anda." crlf)
   (printout t "  Coba ubah pilihan Anda atau tambahkan lebih banyak data wisata." crlf)
   (printout t "=============================================" crlf)
)
