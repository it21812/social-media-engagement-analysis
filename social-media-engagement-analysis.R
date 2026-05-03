# ==========================
# Social Media Engagement Analysis
# ==========================

# --- 1. Φόρτωση Instagram CSV ---
instagram <- read.csv("C:/Users/maria/Desktop/Instagram_Analytics.csv", stringsAsFactors = FALSE)

# --- 2. Δημιουργία engagement για Instagram ---
instagram$engagement <- instagram$likes + instagram$comments + instagram$shares

# --- 3. Φόρτωση TikTok CSV ---
tiktok <- read.csv("C:/Users/maria/Desktop/tiktok_dataset.csv", stringsAsFactors = FALSE)

# --- 4. Δημιουργία engagement για TikTok (καθαρά δεδομένα) ---

tiktok$video_like_count <- as.numeric(gsub(",", "", tiktok$video_like_count))
tiktok$video_comment_count <- as.numeric(gsub(",", "", tiktok$video_comment_count))
tiktok$video_share_count <- as.numeric(gsub(",", "", tiktok$video_share_count))
tiktok$video_view_count <- as.numeric(gsub(",", "", tiktok$video_view_count))

tiktok$engagement <- tiktok$video_like_count +
  tiktok$video_comment_count +
  tiktok$video_share_count
tiktok <- na.omit(tiktok)

# --- 5. Καθαρισμός δεδομένων ---
instagram_clean <- data.frame(
  engagement = as.numeric(instagram$engagement),
  platform = "Instagram"
)

tiktok_clean <- data.frame(
  engagement = as.numeric(tiktok$engagement),
  platform = "TikTok"
)

# --- 6. Ένωση datasets ---
data <- rbind(instagram_clean, tiktok_clean)

# --- 7. Περιγραφική στατιστική ---
aggregate(engagement ~ platform, data = data, mean, na.rm = TRUE)

# --- 8. Εικόνα 1: Boxplot ---
png("C:/Users/maria/Desktop/image1_log.png", width = 800, height = 600)

boxplot(
  log10(engagement + 1) ~ platform,
  data = data,
  main = "Σύγκριση Engagement (Log Scale)",
  ylab = "Log(Engagement)",
  col = c("pink", "lightblue")
)

dev.off()

# --- 9. t-test ---
t.test(engagement ~ platform, data = data)

# --- 10. Εικόνα 2: Verified Status ---
png("C:/Users/maria/Desktop/image2.png", width = 800, height = 600)

boxplot(
  engagement ~ verified_status,
  data = tiktok,
  main = "Engagement ανά Verified Status (TikTok)",
  xlab = "Verified Status",
  ylab = "Engagement",
  col = c("orange", "lightblue")
)

dev.off()

# --- 11. Εικόνα 3: Engagement vs Views ---
png("C:/Users/maria/Desktop/image3.png", width = 800, height = 600)

plot(
  tiktok$video_view_count,
  tiktok$engagement,
  main = "Engagement vs Views",
  xlab = "Views",
  ylab = "Engagement",
  pch = 16,
  col = "blue"
)

abline(lm(engagement ~ video_view_count, data = tiktok), col = "red")

dev.off()