package com.be.rebook.auth.service;

import com.be.rebook.common.exception.BaseException;
import com.be.rebook.common.exception.ErrorCode;
import jakarta.mail.MessagingException;
import jakarta.mail.internet.MimeMessage;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.stereotype.Service;

@Service
public class EmailService {

    private final JavaMailSender mailSender;

    public EmailService(JavaMailSender mailSender){
        this.mailSender = mailSender;
    }

    public void sendVerificationEmail(String to, String verificationCode) {
        try {
            MimeMessage message = mailSender.createMimeMessage();
            MimeMessageHelper helper = new MimeMessageHelper(message, true, "UTF-8");

            helper.setTo(to);
            helper.setSubject("ReBook 인증 코드");

            String htmlContent =
                    "<div style='text-align: center; padding: 20px; border: 2px solid #eee; font-family: Arial, sans-serif;'>"
                            + "<h1 style='font-size: 24px; color: #333;'>ReBook 인증 코드</h1>"
                            + "<div style='margin: 20px auto; width: 200px; background-color: #f9f9f9; padding: 10px; border-radius: 5px;'>"
                            + "<p style='font-size: 36px; font-weight: bold; color: #ff5722; letter-spacing: 10px;'>" + verificationCode + "</p>"
                            + "</div>"
                            + "<p style='font-size: 14px; color: #777;'>이 코드는 3분간 유효합니다.</p>"
                            + "</div>";

            helper.setText(htmlContent, true);

            mailSender.send(message);

        } catch (MessagingException e) {
            e.printStackTrace();
            throw new BaseException(ErrorCode.MAIL_SEND_ERROR);
        }
    }
}